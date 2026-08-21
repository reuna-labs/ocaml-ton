open Ton_adnl

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let unhex s =
  String.init (String.length s / 2) (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let vectors =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/adnl-expected.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "adnl-expected.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"
let to_list = function `List l -> l | _ -> Alcotest.fail "expected a list"
let section n = field n (Lazy.force vectors)

(* --- key identifiers -------------------------------------------------------- *)

let test_key_ids () =
  List.iteri
    (fun i spec ->
      let spec = to_obj spec in
      Alcotest.(check string)
        (Printf.sprintf "key id %d" i)
        (to_str (field "id" spec))
        (hex (Key_id.of_ed25519_pub (unhex (to_str (field "pub" spec))))))
    (to_list (section "keyIds"));
  Alcotest.check_raises "short key" (Invalid_argument "Key_id.of_ed25519_pub: key must be 32 bytes")
    (fun () -> ignore (Key_id.of_ed25519_pub "short"))

(* --- handshake -------------------------------------------------------------- *)

(* The whole packet, byte for byte, against an implementation built separately
   from the protocol description. A handshake that is subtly wrong does not
   fail loudly -- the server simply never answers -- so this is the check that
   matters most in the package. *)
let test_handshake () =
  List.iteri
    (fun i spec ->
      let spec = to_obj spec in
      let server_pub = unhex (to_str (field "serverPub" spec)) in
      let ephemeral_seed = unhex (to_str (field "ephemeralSeed" spec)) in
      let aes_params = unhex (to_str (field "aesParams" spec)) in
      match Handshake.build ~server_pub ~ephemeral_seed ~aes_params with
      | Error e -> Alcotest.failf "handshake %d: %a" i Handshake.pp_error e
      | Ok (packet, rx, tx) ->
          let at f = Printf.sprintf "handshake %d: %s" i f in
          Alcotest.(check int) (at "packet size") Handshake.packet_size (String.length packet);
          Alcotest.(check string) (at "packet") (to_str (field "packet" spec)) (hex packet);
          (* The session keystreams must start where the reference says. *)
          let ks c = let _, s = Ctr.xor c (String.make 16 '\x00') in hex s in
          let expect key iv =
            let _, s = Ctr.xor (Ctr.create ~key:(unhex key) ~iv:(unhex iv)) (String.make 16 '\x00') in
            hex s
          in
          Alcotest.(check string) (at "receive keystream")
            (expect (to_str (field "rxKey" spec)) (to_str (field "rxIv" spec)))
            (ks rx);
          Alcotest.(check string) (at "transmit keystream")
            (expect (to_str (field "txKey" spec)) (to_str (field "txIv" spec)))
            (ks tx))
    (to_list (section "handshakes"))

let first_handshake = lazy (to_obj (List.hd (to_list (section "handshakes"))))

let build_first () =
  let spec = Lazy.force first_handshake in
  Result.get_ok
    (Handshake.build
       ~server_pub:(unhex (to_str (field "serverPub" spec)))
       ~ephemeral_seed:(unhex (to_str (field "ephemeralSeed" spec)))
       ~aes_params:(unhex (to_str (field "aesParams" spec))))

let test_handshake_rejections () =
  let spec = Lazy.force first_handshake in
  let server_pub = unhex (to_str (field "serverPub" spec)) in
  let seed = unhex (to_str (field "ephemeralSeed" spec)) in
  let params = unhex (to_str (field "aesParams" spec)) in
  let err r = match r with Ok _ -> Alcotest.fail "accepted bad input" | Error e -> Format.asprintf "%a" Handshake.pp_error e in
  Alcotest.(check string) "short parameters" "session parameters must be 160 bytes, got 10"
    (err (Handshake.build ~server_pub ~ephemeral_seed:seed ~aes_params:(String.make 10 '\x00')));
  Alcotest.(check string) "short seed" "ephemeral seed: Ed25519 seed must be 32 bytes, got 3"
    (err (Handshake.build ~server_pub ~ephemeral_seed:"abc" ~aes_params:params));
  Alcotest.(check string) "server key with no Montgomery image"
    "server key: Ed25519 public key has no Curve25519 image (y = 1)"
    (err (Handshake.build ~server_pub:(unhex ("01" ^ String.concat "" (List.init 31 (fun _ -> "00"))))
            ~ephemeral_seed:seed ~aes_params:params))

(* --- framing ---------------------------------------------------------------- *)

let test_frames () =
  let spec = Lazy.force first_handshake in
  let _, _, tx = build_first () in
  ignore tx;
  List.iteri
    (fun i f ->
      let f = to_obj f in
      let nonce = unhex (to_str (field "nonce" f)) in
      let payload = unhex (to_str (field "payload" f)) in
      Alcotest.(check string)
        (Printf.sprintf "frame %d plaintext" i)
        (to_str (field "plain" f))
        (hex (Frame.encode ~nonce payload));
      (* Encrypted with a stream starting fresh, as the reference does. *)
      let tx = Ctr.create ~key:(unhex (to_str (field "txKey" spec))) ~iv:(unhex (to_str (field "txIv" spec))) in
      let _, enc = Ctr.xor tx (Frame.encode ~nonce payload) in
      Alcotest.(check string) (Printf.sprintf "frame %d encrypted" i) (to_str (field "encrypted" f)) (hex enc))
    (to_list (section "frames"))

(* Two frames share one keystream. Restarting the counter per frame is the
   obvious mistake and would desynchronise everything after the first. *)
let test_consecutive_frames () =
  let spec = Lazy.force first_handshake in
  let c = to_obj (section "consecutive") in
  let nonces = List.map to_str (to_list (field "nonces" c)) in
  let payloads = List.map to_str (to_list (field "payloads" c)) in
  let tx = Ctr.create ~key:(unhex (to_str (field "txKey" spec))) ~iv:(unhex (to_str (field "txIv" spec))) in
  let tx, a = Ctr.xor tx (Frame.encode ~nonce:(unhex (List.nth nonces 0)) (unhex (List.nth payloads 0))) in
  let _, b = Ctr.xor tx (Frame.encode ~nonce:(unhex (List.nth nonces 1)) (unhex (List.nth payloads 1))) in
  Alcotest.(check string) "one continuous stream" (to_str (field "encrypted" c)) (hex (a ^ b))

let test_frame_decode () =
  let nonce = String.make 32 '\x77' in
  let payload = "hello world" in
  let plain = Frame.encode ~nonce payload in
  (match Frame.decode ~max_size:Frame.default_max_size plain with
  | Ok (Some (p, used)) ->
      Alcotest.(check string) "payload" payload p;
      Alcotest.(check int) "consumed the whole frame" (String.length plain) used
  | Ok None -> Alcotest.fail "reported an incomplete frame"
  | Error e -> Alcotest.failf "%a" Frame.pp_error e);
  (* Every prefix short of the whole frame must be reported as incomplete
     rather than guessed at. *)
  for n = 0 to String.length plain - 1 do
    match Frame.decode ~max_size:Frame.default_max_size (String.sub plain 0 n) with
    | Ok None -> ()
    | Ok (Some _) -> Alcotest.failf "decoded a frame from %d of %d bytes" n (String.length plain)
    | Error e -> Alcotest.failf "prefix of %d bytes: %a" n Frame.pp_error e
  done

let test_frame_rejections () =
  let err s = match Frame.decode ~max_size:Frame.default_max_size s with
    | Error e -> Format.asprintf "%a" Frame.pp_error e
    | Ok _ -> Alcotest.fail "accepted a bad frame"
  in
  Alcotest.(check string) "too small" "frame of 63 bytes is smaller than the 64 byte overhead"
    (err "\063\000\000\000");
  Alcotest.(check string) "too large" "frame of 16777215 bytes exceeds the 4194304 byte limit"
    (err "\255\255\255\000");
  let plain = Frame.encode ~nonce:(String.make 32 '\x01') "payload" in
  let b = Bytes.of_string plain in
  Bytes.set b (Bytes.length b - 1) (Char.chr (Char.code (Bytes.get b (Bytes.length b - 1)) lxor 0xff));
  Alcotest.(check string) "corrupted checksum" "frame checksum does not match" (err (Bytes.to_string b));
  Alcotest.check_raises "short nonce" (Invalid_argument "Frame.encode: nonce must be 32 bytes") (fun () ->
      ignore (Frame.encode ~nonce:"short" ""))

let () =
  Alcotest.run "adnl handshake"
    [ ("key ids", [ Alcotest.test_case "vs reference" `Quick test_key_ids ]);
      ( "handshake",
        [ Alcotest.test_case "packet vs reference" `Quick test_handshake;
          Alcotest.test_case "rejections" `Quick test_handshake_rejections ] );
      ( "framing",
        [ Alcotest.test_case "frames vs reference" `Quick test_frames;
          Alcotest.test_case "consecutive frames share a stream" `Quick test_consecutive_frames;
          Alcotest.test_case "decode and partial input" `Quick test_frame_decode;
          Alcotest.test_case "rejections" `Quick test_frame_rejections ] )
    ]
