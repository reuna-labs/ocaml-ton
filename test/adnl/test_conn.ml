open Ton_adnl

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let server_pub = String.make 32 '\x11'
let ephemeral_seed = String.make 32 '\x22'
let aes_params = String.init 160 (fun i -> Char.chr ((0x40 + i) land 0xff))

let connect ?max_frame () =
  match Conn.connect ?max_frame ~server_pub ~ephemeral_seed ~aes_params () with
  | Ok v -> v
  | Error e -> Alcotest.failf "%a" Conn.pp_error e

(* The peer's stream: the parameters name the two keystreams from the
   client's point of view, so what the client receives is what a server sends
   with the same halves. *)
let peer_stream () =
  Ctr.create ~key:(String.sub aes_params 0 32) ~iv:(String.sub aes_params 64 16)

let as_peer stream frames =
  List.fold_left
    (fun (s, acc) plain ->
      let s, enc = Ctr.xor s plain in
      (s, acc ^ enc))
    (stream, "") frames

let feed conn data =
  match Conn.recv conn data with Ok v -> v | Error e -> Alcotest.failf "%a" Conn.pp_error e

let err conn data =
  match Conn.recv conn data with
  | Ok _ -> Alcotest.fail "accepted a bad frame"
  | Error e -> Format.asprintf "%a" Conn.pp_error e

let test_roundtrip () =
  let conn, packet = connect () in
  Alcotest.(check int) "handshake size" Handshake.packet_size (String.length packet);
  let _, encrypted =
    as_peer (peer_stream ())
      [ Frame.encode ~nonce:(String.make 32 '\x01') "";
        Frame.encode ~nonce:(String.make 32 '\x02') "hello";
        Frame.encode ~nonce:(String.make 32 '\x03') (String.make 500 'x') ]
  in
  let conn, frames = feed conn encrypted in
  Alcotest.(check int) "three frames" 3 (List.length frames);
  Alcotest.(check string) "empty first" "" (List.nth frames 0);
  Alcotest.(check string) "then hello" "hello" (List.nth frames 1);
  Alcotest.(check int) "then a long one" 500 (String.length (List.nth frames 2));
  Alcotest.(check bool) "confirmed" true (Conn.confirmed conn);
  Alcotest.(check int) "nothing buffered" 0 (Conn.buffered conn)

let test_tampered_frame () =
  let conn, _ = connect () in
  let _, encrypted = as_peer (peer_stream ()) [ Frame.encode ~nonce:(String.make 32 '\x01') "payload" ] in
  let b = Bytes.of_string encrypted in
  (* Flip a bit in the payload; the checksum inside the frame must catch it.
     Counter mode gives no integrity of its own, so this is the only thing
     standing between a flipped bit and a corrupted message. *)
  Bytes.set b 40 (Char.chr (Char.code (Bytes.get b 40) lxor 0x01));
  Alcotest.(check string) "checksum" "frame checksum does not match" (err conn (Bytes.to_string b))

let test_oversized_frame () =
  (* A peer choosing how much we allocate is exactly what a small unikernel
     cannot afford, so the limit is checked before the body is read. *)
  let conn, _ = connect ~max_frame:1024 () in
  let plain = "\000\000\016\000" ^ String.make 60 '\x00' in
  let _, encrypted = Ctr.xor (peer_stream ()) plain in
  Alcotest.(check string) "rejected" "frame of 1048576 bytes exceeds the 1024 byte limit"
    (err conn encrypted)

let test_undersized_frame () =
  let conn, _ = connect () in
  let _, encrypted = Ctr.xor (peer_stream ()) "\010\000\000\000" in
  Alcotest.(check string) "rejected" "frame of 10 bytes is smaller than the 64 byte overhead"
    (err conn encrypted)

(* Bytes arrive in whatever sizes the network chose. Every split must produce
   the same frames, including splits inside the encrypted length prefix. *)
let test_every_split () =
  let _, encrypted =
    as_peer (peer_stream ())
      [ Frame.encode ~nonce:(String.make 32 '\x01') "";
        Frame.encode ~nonce:(String.make 32 '\x02') "second frame" ]
  in
  let n = String.length encrypted in
  for split = 0 to n do
    let conn, _ = connect () in
    let conn, a = feed conn (String.sub encrypted 0 split) in
    let _, b = feed conn (String.sub encrypted split (n - split)) in
    let frames = a @ b in
    Alcotest.(check int) (Printf.sprintf "split at %d: two frames" split) 2 (List.length frames);
    Alcotest.(check string) (Printf.sprintf "split at %d: payload" split) "second frame" (List.nth frames 1)
  done

let test_send_is_a_continuous_stream () =
  (* Two sends share one keystream. Restarting per frame would desynchronise
     the peer from the second frame onwards. *)
  let conn, _ = connect () in
  let conn, a = Conn.send conn ~nonce:(String.make 32 '\x0a') "one" in
  let _, b = Conn.send conn ~nonce:(String.make 32 '\x0b') "two" in
  let tx = Ctr.create ~key:(String.sub aes_params 32 32) ~iv:(String.sub aes_params 80 16) in
  let tx, want_a = Ctr.xor tx (Frame.encode ~nonce:(String.make 32 '\x0a') "one") in
  let _, want_b = Ctr.xor tx (Frame.encode ~nonce:(String.make 32 '\x0b') "two") in
  Alcotest.(check string) "first frame" (hex want_a) (hex a);
  Alcotest.(check string) "second frame continues the stream" (hex want_b) (hex b)

let test_send_rejects_short_nonce () =
  let conn, _ = connect () in
  Alcotest.check_raises "short nonce" (Invalid_argument "Frame.encode: nonce must be 32 bytes")
    (fun () -> ignore (Conn.send conn ~nonce:"short" ""))

let () =
  Alcotest.run "adnl conn"
    [ ( "framing",
        [ Alcotest.test_case "receive several frames" `Quick test_roundtrip;
          Alcotest.test_case "every split point" `Quick test_every_split;
          Alcotest.test_case "sending is one stream" `Quick test_send_is_a_continuous_stream;
          Alcotest.test_case "short nonce" `Quick test_send_rejects_short_nonce ] );
      ( "rejections",
        [ Alcotest.test_case "tampered payload" `Quick test_tampered_frame;
          Alcotest.test_case "oversized frame" `Quick test_oversized_frame;
          Alcotest.test_case "undersized frame" `Quick test_undersized_frame ] )
    ]
