open Ton_cell
open Ton_tlb

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let vectors =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/tlb-expected.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "tlb-expected.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_int = function `Int n -> n | _ -> Alcotest.fail "expected an int"
let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"
let section name = to_obj (field name (Lazy.force vectors))
let cell_of b = match Builder.end_cell b with Ok c -> c | Error e -> Alcotest.failf "%a" Builder.pp_error e
let ok_b what = function Ok b -> b | Error e -> Alcotest.failf "%s: %a" what Coins.pp_error e

(* --- coins ---------------------------------------------------------------- *)

let test_coins () =
  List.iter
    (fun (v, spec) ->
      let spec = to_obj spec in
      let z = Z.of_string v in
      let c = cell_of (ok_b v (Coins.store_coins (Builder.create ()) z)) in
      Alcotest.(check string) (v ^ ": hash") (to_str (field "hash" spec)) (hex (Cell.hash c));
      Alcotest.(check int) (v ^ ": bits") (to_int (field "bits" spec)) (Bits.length (Cell.bits c));
      match Slice.parse c (fun s -> Coins.load_coins s) with
      | Ok back -> Alcotest.(check string) (v ^ ": roundtrip") v (Z.to_string back)
      | Error e -> Alcotest.failf "%s: %a" v Slice.pp_error e)
    (section "coins")

let test_varuint () =
  List.iter
    (fun (v, spec) ->
      let spec = to_obj spec in
      let z = Z.of_string v in
      let c = cell_of (ok_b v (Coins.store_var_uint (Builder.create ()) ~n:32 z)) in
      Alcotest.(check string) (v ^ ": hash") (to_str (field "hash" spec)) (hex (Cell.hash c));
      Alcotest.(check int) (v ^ ": bits") (to_int (field "bits" spec)) (Bits.length (Cell.bits c));
      match Slice.parse c (fun s -> Coins.load_var_uint s ~n:32) with
      | Ok back -> Alcotest.(check string) (v ^ ": roundtrip") v (Z.to_string back)
      | Error e -> Alcotest.failf "%s: %a" v Slice.pp_error e)
    (section "varuint")

(* Zero must encode as a zero-length field with no value bits, and every
   amount must use the fewest bytes that fit. Otherwise the encoding is not
   canonical and hashes diverge. *)
let test_coins_canonical () =
  let c = cell_of (ok_b "zero" (Coins.store_coins (Builder.create ()) Z.zero)) in
  Alcotest.(check int) "zero is four bits" 4 (Bits.length (Cell.bits c));
  let c = cell_of (ok_b "255" (Coins.store_coins (Builder.create ()) (Z.of_int 255))) in
  Alcotest.(check int) "255 is one byte" 12 (Bits.length (Cell.bits c));
  let c = cell_of (ok_b "256" (Coins.store_coins (Builder.create ()) (Z.of_int 256))) in
  Alcotest.(check int) "256 is two bytes" 20 (Bits.length (Cell.bits c))

let test_coins_rejects () =
  (match Coins.store_coins (Builder.create ()) (Z.of_int (-1)) with
  | Ok _ -> Alcotest.fail "accepted a negative amount"
  | Error e -> Alcotest.(check string) "negative" "amount -1 is negative" (Format.asprintf "%a" Coins.pp_error e));
  (* VarUInteger 16 holds at most 15 bytes. *)
  match Coins.store_coins (Builder.create ()) (Z.shift_left Z.one 120) with
  | Ok _ -> Alcotest.fail "accepted an oversized amount"
  | Error e ->
      Alcotest.(check bool) "too large" true
        (String.length (Format.asprintf "%a" Coins.pp_error e) > 0
        && String.sub (Format.asprintf "%a" Coins.pp_error e) 0 6 = "amount")

let test_coins_decimal () =
  let check n s = Alcotest.(check string) ("format " ^ n) s (Coins.to_string (Z.of_string n)) in
  check "0" "0";
  check "1" "0.000000001";
  check "1000000000" "1";
  check "1500000000" "1.5";
  check "1234567890" "1.23456789";
  check "-2500000000" "-2.5";
  let parse s = match Coins.of_string s with Ok z -> Z.to_string z | Error e -> "ERR:" ^ e in
  Alcotest.(check string) "parse 1" "1000000000" (parse "1");
  Alcotest.(check string) "parse 1.5" "1500000000" (parse "1.5");
  Alcotest.(check string) "parse .5" "500000000" (parse ".5");
  Alcotest.(check string) "parse -2.5" "-2500000000" (parse "-2.5");
  Alcotest.(check bool) "reject 10 decimals" true
    (match Coins.of_string "1.0000000001" with Error _ -> true | Ok _ -> false);
  Alcotest.(check bool) "reject nonsense" true
    (match Coins.of_string "abc" with Error _ -> true | Ok _ -> false)

(* --- addresses ------------------------------------------------------------ *)

let test_addresses () =
  List.iter
    (fun (name, spec) ->
      let spec = to_obj spec in
      let a =
        match field "raw" spec with
        | `Null -> Msg_address.Addr_none
        | `String raw -> Msg_address.of_address (Result.get_ok (Ton_address.of_raw raw))
        | _ -> Alcotest.fail "bad raw"
      in
      let c = cell_of (Msg_address.store (Builder.create ()) a) in
      Alcotest.(check string) (name ^ ": hash") (to_str (field "hash" spec)) (hex (Cell.hash c));
      Alcotest.(check int) (name ^ ": bits") (to_int (field "bits" spec)) (Bits.length (Cell.bits c));
      match Slice.parse c (fun s -> Msg_address.load s) with
      | Ok back -> Alcotest.(check bool) (name ^ ": roundtrip") true (Msg_address.equal a back)
      | Error e -> Alcotest.failf "%s: %a" name Slice.pp_error e)
    (section "address")

let test_anycast_and_extern () =
  (* Neither appears often, but both are legal and must survive a round-trip. *)
  let ext = Msg_address.Addr_extern (Bits.sub (Bits.of_bytes "\xab\xcd") 0 12) in
  let c = cell_of (Msg_address.store (Builder.create ()) ext) in
  (match Slice.parse c (fun s -> Msg_address.load s) with
  | Ok back -> Alcotest.(check bool) "addr_extern roundtrip" true (Msg_address.equal ext back)
  | Error e -> Alcotest.failf "%a" Slice.pp_error e);
  let any =
    Msg_address.Addr_std
      { anycast = Some { depth = 5; rewrite_pfx = Bits.sub (Bits.of_bytes "\xf8") 0 5 };
        workchain = 0;
        address = String.make 32 '\x11' }
  in
  let c = cell_of (Msg_address.store (Builder.create ()) any) in
  match Slice.parse c (fun s -> Msg_address.load s) with
  | Ok back -> Alcotest.(check bool) "anycast roundtrip" true (Msg_address.equal any back)
  | Error e -> Alcotest.failf "%a" Slice.pp_error e

(* --- state init ----------------------------------------------------------- *)

let code = cell_of (Builder.store_uint (Builder.create ()) 0xdeadbeefL ~bits:32)

let data =
  cell_of (Builder.store_uint (Builder.store_uint (Builder.create ()) 1L ~bits:32) 0x29a9a317L ~bits:32)

let test_state_init () =
  let spec = to_obj (field "code_and_data" (section "stateInit")) in
  Alcotest.(check string) "code hash" (to_str (field "codeHash" spec)) (hex (Cell.hash code));
  Alcotest.(check string) "data hash" (to_str (field "dataHash" spec)) (hex (Cell.hash data));
  let si = { Message.empty_state_init with code = Some code; data = Some data } in
  let c = cell_of (Message.store_state_init (Builder.create ()) si) in
  Alcotest.(check string) "hash" (to_str (field "hash" spec)) (hex (Cell.hash c));
  Alcotest.(check int) "bits" (to_int (field "bits" spec)) (Bits.length (Cell.bits c));
  Alcotest.(check int) "refs" (to_int (field "refs" spec)) (Cell.ref_count c);
  (* The address a contract will have once deployed is the hash of this cell. *)
  let addr w = Result.get_ok (Message.state_init_address ~workchain:w si) in
  Alcotest.(check string) "basechain address" (to_str (field "address0" spec)) (Ton_address.to_raw (addr 0));
  Alcotest.(check string) "masterchain address" (to_str (field "addressMinus1" spec))
    (Ton_address.to_raw (addr (-1)));
  match Slice.parse c (fun s -> Message.load_state_init s) with
  | Ok back ->
      Alcotest.(check bool) "code survives" true (Option.equal Cell.equal back.code (Some code));
      Alcotest.(check bool) "data survives" true (Option.equal Cell.equal back.data (Some data))
  | Error e -> Alcotest.failf "%a" Slice.pp_error e

let test_state_init_empty () =
  let spec = to_obj (field "empty" (section "stateInit")) in
  let c = cell_of (Message.store_state_init (Builder.create ()) Message.empty_state_init) in
  Alcotest.(check string) "hash" (to_str (field "hash" spec)) (hex (Cell.hash c));
  Alcotest.(check int) "bits" (to_int (field "bits" spec)) (Bits.length (Cell.bits c))

(* --- messages ------------------------------------------------------------- *)

let src = Result.get_ok (Ton_address.of_raw "0:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76")
let dest = Result.get_ok (Ton_address.of_raw "-1:34517c7bdf5187c55af4f8b61fdc321588c7ab768dee24b006df29106458d7cf")

let body =
  cell_of (Builder.store_bytes (Builder.store_uint (Builder.create ()) 0L ~bits:32) "hello")

let ok_m what = function Ok v -> v | Error e -> Alcotest.failf "%s: %s" what e

let check_message name msg =
  let spec = to_obj (field name (section "message")) in
  let c = ok_m name (Message.to_cell msg) in
  Alcotest.(check string) (name ^ ": hash") (to_str (field "hash" spec)) (hex (Cell.hash c));
  Alcotest.(check int) (name ^ ": bits") (to_int (field "bits" spec)) (Bits.length (Cell.bits c));
  Alcotest.(check int) (name ^ ": refs") (to_int (field "refs" spec)) (Cell.ref_count c);
  match Message.of_cell c with
  | Error e -> Alcotest.failf "%s: reparse: %a" name Slice.pp_error e
  | Ok back ->
      Alcotest.(check string) (name ^ ": body survives") (hex (Cell.hash msg.Message.body))
        (hex (Cell.hash back.Message.body));
      back

let internal_info ?(bounce = true) ?(lt = 0L) ?(at = 0l) coins =
  Message.Internal
    { ihr_disabled = true; bounce; bounced = false;
      src = Msg_address.of_address src; dest = Msg_address.of_address dest;
      value = Currency.of_coins (Z.of_int coins); ihr_fee = Z.zero; fwd_fee = Z.zero;
      created_lt = lt; created_at = at }

let test_message_internal () =
  let back = check_message "internal" { Message.info = internal_info 1000000000; init = None; body } in
  match back.Message.info with
  | Message.Internal i ->
      Alcotest.(check string) "value" "1000000000" (Z.to_string i.value.Currency.coins);
      Alcotest.(check bool) "bounce" true i.bounce;
      Alcotest.(check bool) "dest" true
        (Msg_address.equal i.dest (Msg_address.of_address dest))
  | _ -> Alcotest.fail "expected an internal message"

let test_message_external_in () =
  let info = Message.External_in { src = Msg_address.Addr_none; dest = Msg_address.of_address dest; import_fee = Z.zero } in
  let back = check_message "external_in" { Message.info; init = None; body } in
  match back.Message.info with
  | Message.External_in i -> Alcotest.(check bool) "src is none" true (i.src = Msg_address.Addr_none)
  | _ -> Alcotest.fail "expected an external-in message"

let test_message_with_init () =
  let info = Message.External_in { src = Msg_address.Addr_none; dest = Msg_address.of_address dest; import_fee = Z.zero } in
  let init = Some { Message.empty_state_init with code = Some code; data = Some data } in
  let back = check_message "external_in_with_init" { Message.info; init; body } in
  match back.Message.init with
  | Some si -> Alcotest.(check bool) "code survives" true (Option.equal Cell.equal si.code (Some code))
  | None -> Alcotest.fail "state init was lost"

(* A body too large to inline must go behind a reference; the reference SDK
   makes the same choice, so the hashes must still agree. *)
let test_message_big_body () =
  let big = cell_of (Builder.store_bytes (Builder.create ()) (String.make 120 '\xab')) in
  let back =
    check_message "internal_big_body"
      { Message.info = internal_info ~bounce:false ~lt:7L ~at:1700000000l 42; init = None; body = big }
  in
  ignore back

let () =
  Alcotest.run "tlb"
    [ ( "coins",
        [ Alcotest.test_case "vs reference" `Quick test_coins;
          Alcotest.test_case "var uint vs reference" `Quick test_varuint;
          Alcotest.test_case "canonical widths" `Quick test_coins_canonical;
          Alcotest.test_case "rejections" `Quick test_coins_rejects;
          Alcotest.test_case "decimal formatting" `Quick test_coins_decimal ] );
      ( "addresses",
        [ Alcotest.test_case "vs reference" `Quick test_addresses;
          Alcotest.test_case "anycast and extern" `Quick test_anycast_and_extern ] );
      ( "state init",
        [ Alcotest.test_case "code and data" `Quick test_state_init;
          Alcotest.test_case "empty" `Quick test_state_init_empty ] );
      ( "messages",
        [ Alcotest.test_case "internal" `Quick test_message_internal;
          Alcotest.test_case "external in" `Quick test_message_external_in;
          Alcotest.test_case "with state init" `Quick test_message_with_init;
          Alcotest.test_case "body too large to inline" `Quick test_message_big_body ] )
    ]
