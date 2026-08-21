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

let expected =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/account-expected.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "account-expected.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_int = function `Int n -> n | _ -> Alcotest.fail "expected an int"
let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"

let to_str_opt = function `Null -> None | `String s -> Some s | _ -> Alcotest.fail "expected string or null"

let load_boc name =
  let raw = read_file ("../vectors/ton-core/" ^ name) in
  let src =
    if String.length raw > 3 && String.sub raw 0 3 = "te6" then
      match Base64.decode (String.trim raw) with
      | Ok s -> s
      | Error (`Msg m) -> Alcotest.failf "%s: %s" name m
    else raw
  in
  match Boc.deserialize_root src with
  | Ok c -> c
  | Error e -> Alcotest.failf "%s: %a" name Boc.pp_error e

(* Parsing a real mainnet account and then re-serialising it must reproduce
   the exact cell we started from. That is a much stronger statement than
   "the fields look right": every optional, every variable-length integer and
   every tag has to be both read and written correctly, or the hash moves. *)
let case name () =
  let spec = to_obj (field name (Lazy.force expected)) in
  let cell = load_boc name in
  Alcotest.(check string) "fixture root hash" (to_str (field "rootHash" spec)) (hex (Cell.hash cell));
  let a =
    match Account.of_cell cell with
    | Ok (Some a) -> a
    | Ok None -> Alcotest.fail "expected an account, got account_none"
    | Error e -> Alcotest.failf "%s: %a" name Slice.pp_error e
  in
  let at f = Printf.sprintf "%s: %s" name f in
  Alcotest.(check (option string))
    (at "address")
    (Some (to_str (field "addr" spec)))
    (Option.map Ton_address.to_raw (Account.address a));
  let used = to_obj (field "used" spec) in
  Alcotest.(check string) (at "used cells") (to_str (field "cells" used))
    (Z.to_string a.Account.storage_info.used.cells);
  Alcotest.(check string) (at "used bits") (to_str (field "bits" used))
    (Z.to_string a.Account.storage_info.used.bits);
  Alcotest.(check (option string)) (at "storage extra") (to_str_opt (field "storageExtra" spec))
    (Option.map Z.to_string a.Account.storage_info.storage_extra);
  Alcotest.(check int) (at "last paid") (to_int (field "lastPaid" spec))
    (Int32.to_int a.Account.storage_info.last_paid);
  Alcotest.(check (option string)) (at "due payment") (to_str_opt (field "duePayment" spec))
    (Option.map Z.to_string a.Account.storage_info.due_payment);
  Alcotest.(check string) (at "last trans lt") (to_str (field "lastTransLt" spec))
    (Int64.to_string a.Account.storage.last_trans_lt);
  Alcotest.(check string) (at "balance") (to_str (field "balance" spec)) (Z.to_string (Account.balance a));
  Alcotest.(check string) (at "state") (to_str (field "stateType" spec))
    (match a.Account.storage.state with
    | Account.Uninit -> "uninit"
    | Account.Active _ -> "active"
    | Account.Frozen _ -> "frozen");
  Alcotest.(check (option string)) (at "code hash") (to_str_opt (field "codeHash" spec))
    (Option.map (fun c -> hex (Cell.hash c)) (Account.code a));
  Alcotest.(check (option string)) (at "data hash") (to_str_opt (field "dataHash" spec))
    (Option.map (fun c -> hex (Cell.hash c)) (Account.data a));
  (* The round-trip. *)
  match Account.store (Builder.create ()) (Some a) with
  | Error e -> Alcotest.failf "%s: store: %s" name e
  | Ok b -> (
      match Builder.end_cell b with
      | Error e -> Alcotest.failf "%s: %a" name Builder.pp_error e
      | Ok re ->
          Alcotest.(check string) (at "re-serialised hash") (to_str (field "reserializedHash" spec))
            (hex (Cell.hash re));
          Alcotest.(check string) (at "re-serialises to the original cell") (hex (Cell.hash cell))
            (hex (Cell.hash re)))

let case_names = lazy (List.map fst (Lazy.force expected))

(* --- synthetic ------------------------------------------------------------ *)

let test_account_none () =
  let c = Result.get_ok (Builder.end_cell (Result.get_ok (Account.store (Builder.create ()) None))) in
  Alcotest.(check int) "a single zero bit" 1 (Bits.length (Cell.bits c));
  match Account.of_cell c with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "expected account_none"
  | Error e -> Alcotest.failf "%a" Slice.pp_error e

let roundtrip name a =
  let c =
    Result.get_ok (Builder.end_cell (Result.get_ok (Account.store (Builder.create ()) (Some a))))
  in
  match Account.of_cell c with
  | Ok (Some back) ->
      let c2 =
        Result.get_ok (Builder.end_cell (Result.get_ok (Account.store (Builder.create ()) (Some back))))
      in
      Alcotest.(check string) (name ^ ": stable") (hex (Cell.hash c)) (hex (Cell.hash c2));
      back
  | Ok None -> Alcotest.failf "%s: lost the account" name
  | Error e -> Alcotest.failf "%s: %a" name Slice.pp_error e

let base_account state =
  { Account.addr = Msg_address.of_address (Result.get_ok (Ton_address.of_raw "0:0000000000000000000000000000000000000000000000000000000000000001"));
    storage_info =
      { used = { cells = Z.of_int 3; bits = Z.of_int 500 };
        storage_extra = None;
        last_paid = 1700000000l;
        due_payment = None };
    storage = { last_trans_lt = 42L; balance = Currency.of_coins (Z.of_int 1000); state } }

let test_uninit () =
  let back = roundtrip "uninit" (base_account Account.Uninit) in
  Alcotest.(check bool) "not active" false (Account.is_active back);
  Alcotest.(check bool) "no code" true (Account.code back = None)

let test_frozen () =
  let h = String.make 32 '\x7f' in
  let back = roundtrip "frozen" (base_account (Account.Frozen h)) in
  match back.Account.storage.state with
  | Account.Frozen h' -> Alcotest.(check string) "state hash survives" (hex h) (hex h')
  | _ -> Alcotest.fail "expected a frozen account"

let test_due_payment_and_extra () =
  (* Both optional fields present at once, which the mainnet fixtures do not
     exercise. *)
  let a = base_account Account.Uninit in
  let a =
    { a with
      Account.storage_info =
        { a.Account.storage_info with
          storage_extra = Some (Z.of_string "12345678901234567890");
          due_payment = Some (Z.of_int 999) } }
  in
  let back = roundtrip "extras" a in
  Alcotest.(check (option string)) "storage extra" (Some "12345678901234567890")
    (Option.map Z.to_string back.Account.storage_info.storage_extra);
  Alcotest.(check (option string)) "due payment" (Some "999")
    (Option.map Z.to_string back.Account.storage_info.due_payment)

let test_shard_account () =
  let a = base_account Account.Uninit in
  let sh = { Account.account = Some a; last_trans_hash = String.make 32 '\x01'; last_trans_lt = 7L } in
  let c = Result.get_ok (Builder.end_cell (Result.get_ok (Account.store_shard (Builder.create ()) sh))) in
  match Slice.parse c (fun s -> Account.load_shard s) with
  | Ok back ->
      Alcotest.(check string) "last trans hash" (hex sh.Account.last_trans_hash) (hex back.Account.last_trans_hash);
      Alcotest.(check int64) "last trans lt" 7L back.Account.last_trans_lt;
      Alcotest.(check bool) "account survives" true (back.Account.account <> None)
  | Error e -> Alcotest.failf "%a" Slice.pp_error e

let () =
  Alcotest.run "account"
    [ ("mainnet states", List.map (fun n -> Alcotest.test_case n `Quick (case n)) (Lazy.force case_names));
      ( "synthetic",
        [ Alcotest.test_case "account_none" `Quick test_account_none;
          Alcotest.test_case "uninit" `Quick test_uninit;
          Alcotest.test_case "frozen" `Quick test_frozen;
          Alcotest.test_case "due payment and storage extra" `Quick test_due_payment_and_extra;
          Alcotest.test_case "shard account" `Quick test_shard_account ] )
    ]
