open Ton_address

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let expected =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/address-expected.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "address-expected.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_int = function `Int n -> n | _ -> Alcotest.fail "expected an int"
let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let ok what = function Ok v -> v | Error e -> Alcotest.failf "%s: %a" what pp_error e

(* Every raw address must survive a trip through all eight textual forms and
   come back identical, with the presentation flags recovered intact. *)
let case raw () =
  let spec = to_obj (field raw (Lazy.force expected)) in
  let a = ok raw (of_raw raw) in
  Alcotest.(check int) "workchain" (to_int (field "workchain" spec)) a.workchain;
  Alcotest.(check string) "hash" (to_str (field "hash" spec)) (hex a.hash);
  Alcotest.(check string) "raw round-trip" (to_str (field "rawString" spec)) (to_raw a);
  List.iter
    (fun (key, want) ->
      let want = to_str want in
      (* key looks like "bounceable=true,testnet=false,urlSafe=true" *)
      let flag name =
        List.exists
          (fun kv -> kv = name ^ "=true")
          (String.split_on_char ',' key)
      in
      let bounceable = flag "bounceable" and testnet = flag "testnet" and url_safe = flag "urlSafe" in
      Alcotest.(check string)
        (Printf.sprintf "encode %s" key)
        want
        (to_friendly ~bounceable ~testnet ~url_safe a);
      let f = ok key (of_friendly want) in
      Alcotest.(check bool) (key ^ ": address") true (equal a f.address);
      Alcotest.(check bool) (key ^ ": bounceable") bounceable f.bounceable;
      Alcotest.(check bool) (key ^ ": testnet") testnet f.testnet;
      (* of_string accepts either form. *)
      Alcotest.(check bool) (key ^ ": of_string") true (equal a (ok key (of_string want))))
    (to_obj (field "forms" spec));
  Alcotest.(check bool) "of_string on raw" true (equal a (ok raw (of_string raw)))

let case_names = lazy (List.map fst (Lazy.force expected))

(* Both alphabets must be accepted on input regardless of which was used. *)
let test_alphabet_agnostic () =
  let a = ok "raw" (of_raw "0:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76") in
  let url = to_friendly ~url_safe:true a and std = to_friendly ~url_safe:false a in
  Alcotest.(check bool) "the two alphabets differ here" false (String.equal url std);
  Alcotest.(check bool) "url-safe parses" true (equal a (ok "url" (of_friendly url)).address);
  Alcotest.(check bool) "standard parses" true (equal a (ok "std" (of_friendly std)).address)

let test_defaults () =
  let a = ok "raw" (of_raw "0:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76") in
  Alcotest.(check string) "default form is bounceable, mainnet, url-safe"
    (to_friendly ~bounceable:true ~testnet:false ~url_safe:true a)
    (to_friendly a);
  Alcotest.(check bool) "and starts with EQ" true (String.length (to_friendly a) > 2 && String.sub (to_friendly a) 0 2 = "EQ")

(* --- rejections ----------------------------------------------------------- *)

let rejects name input expected_msg =
  Alcotest.test_case name `Quick (fun () ->
      match of_string input with
      | Ok _ -> Alcotest.failf "accepted %S" input
      | Error e -> Alcotest.(check string) "error" expected_msg (Format.asprintf "%a" pp_error e))

let corrupt s i =
  let b = Bytes.of_string s in
  let c = Bytes.get b i in
  Bytes.set b i (if c = 'A' then 'B' else 'A');
  Bytes.to_string b

let test_bad_crc () =
  let a = ok "raw" (of_raw "0:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76") in
  let s = to_friendly a in
  (* Flip a character in the hash body; the checksum must catch it. *)
  match of_friendly (corrupt s 10) with
  | Ok _ -> Alcotest.fail "accepted an address with a corrupted body"
  | Error (Bad_crc _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_error e

let test_bad_tag () =
  (* Valid length and CRC, but a tag byte that means nothing. *)
  let body = String.concat "" [ "\x99"; "\x00"; String.make 32 '\x00' ] in
  let s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet (body ^ Web3_codec.Crc.crc16_xmodem_be body) in
  match of_friendly s with
  | Ok _ -> Alcotest.fail "accepted an unknown tag"
  | Error e -> Alcotest.(check string) "error" "unknown address tag 0x99" (Format.asprintf "%a" pp_error e)

let test_rejections =
  [ rejects "empty" "" "expected 36 bytes, got 0";
    rejects "not base64 at all" "abc" "not valid base64";
    rejects "valid base64, wrong length" "abcd" "expected 36 bytes, got 3";
    rejects "raw with short hash" "0:1234" "not a raw address: \"0:1234\"";
    rejects "raw with non-hex" "0:zzd954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76"
      "not a raw address: \"0:zzd954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76\"";
    rejects "raw with bad workchain" "x:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76"
      "not a raw address: \"x:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76\"";
    Alcotest.test_case "corrupted checksum" `Quick test_bad_crc;
    Alcotest.test_case "unknown tag" `Quick test_bad_tag ]

let test_make () =
  Alcotest.(check bool) "32 bytes is fine" true (Result.is_ok (make ~workchain:0 ~hash:(String.make 32 '\x00')));
  match make ~workchain:0 ~hash:"short" with
  | Ok _ -> Alcotest.fail "accepted a short hash"
  | Error e -> Alcotest.(check string) "error" "expected 36 bytes, got 5" (Format.asprintf "%a" pp_error e)

let () =
  Alcotest.run "address"
    [ ("vs reference", List.map (fun n -> Alcotest.test_case n `Quick (case n)) (Lazy.force case_names));
      ( "encoding",
        [ Alcotest.test_case "both alphabets accepted" `Quick test_alphabet_agnostic;
          Alcotest.test_case "defaults" `Quick test_defaults;
          Alcotest.test_case "make" `Quick test_make ] );
      ("rejections", test_rejections)
    ]
