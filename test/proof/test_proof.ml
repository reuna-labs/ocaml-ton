open Ton_cell
open Ton_proof

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
    (match Yojson.Safe.from_string (read_file "../vectors/account-proofs.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "account-proofs.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"
let accounts = lazy (to_obj (field "accounts" (Lazy.force vectors)))
let b64 s = match Base64.decode s with Ok v -> v | Error (`Msg m) -> Alcotest.fail m

type case = {
  name : string;
  address : Ton_address.t;
  shard_root : string;
  state : string;
  proof : string;
  shard_proof : string;
}

let case name =
  let spec = to_obj (field name (Lazy.force accounts)) in
  { name;
    address = Result.get_ok (Ton_address.of_raw (to_str (field "address" spec)));
    shard_root = unhex (to_str (field "rootHash" (to_obj (field "shardblk" spec))));
    state = b64 (to_str (field "state" spec));
    proof = b64 (to_str (field "proof" spec));
    shard_proof = b64 (to_str (field "shardProof" spec)) }

let case_names = lazy (List.map fst (Lazy.force accounts))

(* --- the proofs verify -------------------------------------------------------- *)

(* Real answers from a mainnet liteserver, checked against the block root hash
   it named. This is the difference between reading what a server said and
   knowing it follows from a block. *)
let test_verifies name () =
  let c = case name in
  match Account.verify ~block_root_hash:c.shard_root ~proof:c.proof ~state:c.state ~address:c.address with
  | Error e -> Alcotest.failf "%s: %a" name Account.pp_error e
  | Ok (Account.Exists cell) ->
      Alcotest.(check bool) (name ^ ": a state was returned") true (c.state <> "");
      (* The verified cell is the one the server sent, and it parses. *)
      Alcotest.(check string) (name ^ ": is the state we were given") (hex (Cell.hash (Result.get_ok (Boc.deserialize_root c.state)))) (hex (Cell.hash cell));
      (match Ton_tlb.Account.of_cell cell with
      | Ok (Some a) ->
          Alcotest.(check bool) (name ^ ": has a balance") true (Z.sign (Ton_tlb.Account.balance a) > 0);
          Alcotest.(check (option string)) (name ^ ": address matches")
            (Some (Ton_address.to_raw c.address))
            (Option.map Ton_address.to_raw (Ton_tlb.Account.address a))
      | Ok None -> Alcotest.failf "%s: verified cell is account_none" name
      | Error e -> Alcotest.failf "%s: %a" name Slice.pp_error e)
  | Ok Account.Does_not_exist ->
      Alcotest.(check string) (name ^ ": no state was returned") "" c.state

(* Absence is a claim that has to be proved too. The proof must show the path
   where the account would sit, so that a server cannot deny an account by
   omitting the subtree it lives in. *)
let test_absence () =
  let c = case "basechain_absent" in
  Alcotest.(check string) "the server returned no state" "" c.state;
  match Account.verify ~block_root_hash:c.shard_root ~proof:c.proof ~state:c.state ~address:c.address with
  | Ok Account.Does_not_exist -> ()
  | Ok (Account.Exists _) -> Alcotest.fail "claimed the account exists"
  | Error e -> Alcotest.failf "%a" Account.pp_error e

(* --- tampering is caught ------------------------------------------------------ *)

let expect_error name r =
  match r with
  | Ok _ -> Alcotest.failf "%s: accepted" name
  | Error e -> Format.asprintf "%a" Account.pp_error e

(* Aimed at the wrong block, the proof no longer applies -- this is what stops
   a server replaying an old, valid proof for a state that has since changed. *)
let test_wrong_block () =
  let c = case "elector" in
  let other = String.make 32 '\xab' in
  let msg =
    expect_error "wrong block"
      (Account.verify ~block_root_hash:other ~proof:c.proof ~state:c.state ~address:c.address)
  in
  Alcotest.(check bool) "reports the missing block proof" true
    (String.length msg > 20 && String.sub msg 0 8 = "no proof")

(* Substituting a different account's state must fail on the committed hash. *)
let test_swapped_state () =
  let elector = case "elector" and config = case "config" in
  let msg =
    expect_error "swapped state"
      (Account.verify ~block_root_hash:elector.shard_root ~proof:elector.proof ~state:config.state
         ~address:elector.address)
  in
  Alcotest.(check bool) "reports a hash mismatch" true
    (String.length msg > 14 && String.sub msg 0 14 = "hash mismatch:")

(* Flipping a byte inside the account state must break the committed hash. *)
let test_tampered_state () =
  let c = case "elector" in
  let cell = Result.get_ok (Boc.deserialize_root c.state) in
  let bits = Cell.bits cell in
  let tampered =
    let b = Builder.create () in
    let b = Builder.store_bit b (not (Bits.get bits 0)) in
    let b = Builder.store_bits b (Bits.sub bits 1 (Bits.length bits - 1)) in
    let b = List.fold_left Builder.store_ref b (Cell.refs cell) in
    Boc.serialize (Result.get_ok (Builder.end_cell b))
  in
  let msg =
    expect_error "tampered state"
      (Account.verify ~block_root_hash:c.shard_root ~proof:c.proof ~state:tampered ~address:c.address)
  in
  Alcotest.(check bool) "reports a hash mismatch" true
    (String.length msg > 14 && String.sub msg 0 14 = "hash mismatch:")

(* Claiming a state for an account the proof shows to be absent. *)
let test_state_for_absent_account () =
  let absent = case "basechain_absent" and elector = case "elector" in
  let msg =
    expect_error "state for an absent account"
      (Account.verify ~block_root_hash:absent.shard_root ~proof:absent.proof ~state:elector.state
         ~address:absent.address)
  in
  Alcotest.(check string) "reported" "the proof says the account is absent but a state was supplied" msg

(* Asking about an address the proof does not cover must not be answered. This
   is the case a naive lookup gets wrong: the key is not in the part of the
   dictionary we were shown, which is not the same as not existing. *)
let test_uncovered_address () =
  let c = case "elector" in
  let other =
    Result.get_ok
      (Ton_address.make ~workchain:(-1) ~hash:(String.make 32 '\x7e'))
  in
  match Account.verify ~block_root_hash:c.shard_root ~proof:c.proof ~state:"" ~address:other with
  | Ok Account.Does_not_exist -> Alcotest.fail "claimed absence without a proof of it"
  | Ok (Account.Exists _) -> Alcotest.fail "claimed existence"
  | Error (Account.Elided _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" Account.pp_error e

let test_wrong_workchain () =
  let c = case "elector" in
  let wrong = Result.get_ok (Ton_address.make ~workchain:0 ~hash:c.address.Ton_address.hash) in
  let msg =
    expect_error "wrong workchain"
      (Account.verify ~block_root_hash:c.shard_root ~proof:c.proof ~state:c.state ~address:wrong)
  in
  Alcotest.(check string) "reported" "proof is for workchain -1, address is in 0" msg

(* --- merkle primitives --------------------------------------------------------- *)

let test_merkle_accessors () =
  let c = case "elector" in
  let roots = Result.get_ok (Boc.deserialize c.proof) in
  Alcotest.(check int) "two roots" 2 (List.length roots);
  List.iter
    (fun r ->
      match Merkle.proof r with
      | Ok p ->
          Alcotest.(check int) "committed hash is 32 bytes" 32 (String.length p.Merkle.hash);
          (* Cell construction already checked this, which is why a proof that
             parses is internally consistent. *)
          Alcotest.(check string) "commits to its own child" (hex p.Merkle.hash)
            (hex (Cell.hash p.Merkle.root));
          Alcotest.(check int) "depth matches" (Cell.depth p.Merkle.root) p.Merkle.depth
      | Error e -> Alcotest.failf "%a" Merkle.pp_error e)
    roots;
  (* An ordinary cell is not a proof. *)
  match Merkle.proof Cell.empty with
  | Ok _ -> Alcotest.fail "accepted an ordinary cell"
  | Error e ->
      Alcotest.(check string) "error" "expected a merkle proof cell, got ordinary"
        (Format.asprintf "%a" Merkle.pp_error e)

let () =
  Alcotest.run "proof"
    [ ( "mainnet answers",
        List.map (fun n -> Alcotest.test_case n `Quick (test_verifies n)) (Lazy.force case_names)
        @ [ Alcotest.test_case "absence is proved" `Quick test_absence ] );
      ( "tampering",
        [ Alcotest.test_case "wrong block" `Quick test_wrong_block;
          Alcotest.test_case "swapped state" `Quick test_swapped_state;
          Alcotest.test_case "tampered state" `Quick test_tampered_state;
          Alcotest.test_case "state for an absent account" `Quick test_state_for_absent_account;
          Alcotest.test_case "address the proof does not cover" `Quick test_uncovered_address;
          Alcotest.test_case "wrong workchain" `Quick test_wrong_workchain ] );
      ("merkle", [ Alcotest.test_case "accessors" `Quick test_merkle_accessors ])
    ]
