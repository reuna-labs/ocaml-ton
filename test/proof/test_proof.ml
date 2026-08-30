open Ton_cell
open Ton_proof

let hex s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let unhex s =
  String.init
    (String.length s / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let vectors =
  lazy
    (match
       Yojson.Safe.from_string (read_file "../vectors/account-proofs.json")
     with
    | `Assoc l -> l
    | _ -> Alcotest.fail "account-proofs.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with
  | Some v -> v
  | None -> Alcotest.failf "missing field %S" name

let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"
let accounts = lazy (to_obj (field "accounts" (Lazy.force vectors)))

let b64 s =
  match Base64.decode s with Ok v -> v | Error (`Msg m) -> Alcotest.fail m

type case = {
  address : Ton_address.t;
  shard_root : string;
  state : string;
  proof : string;
  shard_proof : string;
}

let case name =
  let spec = to_obj (field name (Lazy.force accounts)) in
  {
    address = Result.get_ok (Ton_address.of_raw (to_str (field "address" spec)));
    shard_root =
      unhex (to_str (field "rootHash" (to_obj (field "shardblk" spec))));
    state = b64 (to_str (field "state" spec));
    proof = b64 (to_str (field "proof" spec));
    shard_proof = b64 (to_str (field "shardProof" spec));
  }

let case_names = lazy (List.map fst (Lazy.force accounts))

(* --- the proofs verify -------------------------------------------------------- *)

(* Real answers from a mainnet liteserver, checked against the block root hash
   it named. This is the difference between reading what a server said and
   knowing it follows from a block. *)
let test_verifies name () =
  let c = case name in
  match
    Account.verify ~block_root_hash:c.shard_root ~proof:c.proof ~state:c.state
      ~address:c.address
  with
  | Error e -> Alcotest.failf "%s: %a" name Account.pp_error e
  | Ok (Account.Exists cell) -> (
      Alcotest.(check bool)
        (name ^ ": a state was returned")
        true (c.state <> "");
      (* The verified cell is the one the server sent, and it parses. *)
      Alcotest.(check string)
        (name ^ ": is the state we were given")
        (hex (Cell.hash (Result.get_ok (Boc.deserialize_root c.state))))
        (hex (Cell.hash cell));
      match Ton_tlb.Account.of_cell cell with
      | Ok (Some a) ->
          Alcotest.(check bool)
            (name ^ ": has a balance") true
            (Z.sign (Ton_tlb.Account.balance a) > 0);
          Alcotest.(check (option string))
            (name ^ ": address matches")
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
  match
    Account.verify ~block_root_hash:c.shard_root ~proof:c.proof ~state:c.state
      ~address:c.address
  with
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
      (Account.verify ~block_root_hash:other ~proof:c.proof ~state:c.state
         ~address:c.address)
  in
  Alcotest.(check bool)
    "reports the missing block proof" true
    (String.length msg > 20 && String.sub msg 0 8 = "no proof")

(* Substituting a different account's state must fail on the committed hash. *)
let test_swapped_state () =
  let elector = case "elector" and config = case "config" in
  let msg =
    expect_error "swapped state"
      (Account.verify ~block_root_hash:elector.shard_root ~proof:elector.proof
         ~state:config.state ~address:elector.address)
  in
  Alcotest.(check bool)
    "reports a hash mismatch" true
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
      (Account.verify ~block_root_hash:c.shard_root ~proof:c.proof
         ~state:tampered ~address:c.address)
  in
  Alcotest.(check bool)
    "reports a hash mismatch" true
    (String.length msg > 14 && String.sub msg 0 14 = "hash mismatch:")

(* Claiming a state for an account the proof shows to be absent. *)
let test_state_for_absent_account () =
  let absent = case "basechain_absent" and elector = case "elector" in
  let msg =
    expect_error "state for an absent account"
      (Account.verify ~block_root_hash:absent.shard_root ~proof:absent.proof
         ~state:elector.state ~address:absent.address)
  in
  Alcotest.(check string)
    "reported" "the proof says the account is absent but a state was supplied"
    msg

(* Asking about an address the proof does not cover must not be answered. This
   is the case a naive lookup gets wrong: the key is not in the part of the
   dictionary we were shown, which is not the same as not existing. *)
let test_uncovered_address () =
  let c = case "elector" in
  let other =
    Result.get_ok
      (Ton_address.make ~workchain:(-1) ~hash:(String.make 32 '\x7e'))
  in
  match
    Account.verify ~block_root_hash:c.shard_root ~proof:c.proof ~state:""
      ~address:other
  with
  | Ok Account.Does_not_exist ->
      Alcotest.fail "claimed absence without a proof of it"
  | Ok (Account.Exists _) -> Alcotest.fail "claimed existence"
  | Error (Account.Elided _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" Account.pp_error e

let test_wrong_workchain () =
  let c = case "elector" in
  let wrong =
    Result.get_ok
      (Ton_address.make ~workchain:0 ~hash:c.address.Ton_address.hash)
  in
  let msg =
    expect_error "wrong workchain"
      (Account.verify ~block_root_hash:c.shard_root ~proof:c.proof
         ~state:c.state ~address:wrong)
  in
  Alcotest.(check string)
    "reported" "proof is for workchain -1, address is in 0" msg

(* --- merkle primitives --------------------------------------------------------- *)

let test_merkle_accessors () =
  let c = case "elector" in
  let roots = Result.get_ok (Boc.deserialize c.proof) in
  Alcotest.(check int) "two roots" 2 (List.length roots);
  List.iter
    (fun r ->
      match Merkle.proof r with
      | Ok p ->
          Alcotest.(check int)
            "committed hash is 32 bytes" 32
            (String.length p.Merkle.hash);
          (* Cell construction already checked this, which is why a proof that
             parses is internally consistent. *)
          Alcotest.(check string)
            "commits to its own child" (hex p.Merkle.hash)
            (hex (Cell.hash p.Merkle.root));
          Alcotest.(check int)
            "depth matches" (Cell.depth p.Merkle.root) p.Merkle.depth
      | Error e -> Alcotest.failf "%a" Merkle.pp_error e)
    roots;
  (* An ordinary cell is not a proof. *)
  match Merkle.proof Cell.empty with
  | Ok _ -> Alcotest.fail "accepted an ordinary cell"
  | Error e ->
      Alcotest.(check string)
        "error" "expected a merkle proof cell, got ordinary"
        (Format.asprintf "%a" Merkle.pp_error e)

(* --- the shard link ---------------------------------------------------------- *)

let mc_block = lazy (to_obj (field "block" (Lazy.force vectors)))
let mc_root () = unhex (to_str (field "rootHash" (Lazy.force mc_block)))

let shardblk name =
  let spec = to_obj (field name (Lazy.force accounts)) in
  to_obj (field "shardblk" spec)

let int_of_json = function
  | `Int n -> Int32.of_int n
  | `Intlit s -> Int32.of_string s
  | _ -> Alcotest.fail "expected an int"

(* An account outside the masterchain is proved against a shard block that the
   server also chose. Without this step the chain of trust has a hole exactly
   the size of "which shard block did you mean". *)
let test_shard_link () =
  let c = case "basechain_absent" in
  let sb = shardblk "basechain_absent" in
  let workchain = int_of_json (field "workchain" sb) in
  let shard = Int64.of_string ("0x" ^ to_str (field "shard" sb)) in
  let root_hash = unhex (to_str (field "rootHash" sb)) in
  let seqno = int_of_json (field "seqno" sb) in
  match
    Shard.find ~mc_root_hash:(mc_root ()) ~shard_proof:c.shard_proof ~workchain
      ~shard
  with
  | Error e -> Alcotest.failf "%a" Shard.pp_error e
  | Ok d ->
      Alcotest.(check string)
        "the masterchain records this shard block" (hex root_hash)
        (hex d.Shard.root_hash);
      Alcotest.(check int32) "and this seqno" seqno d.Shard.seqno;
      Alcotest.(check int)
        "file hash is 32 bytes" 32
        (String.length d.Shard.file_hash);
      (* And the same through the checking entry point. *)
      Alcotest.(check bool)
        "verify agrees" true
        (Result.is_ok
           (Shard.verify ~mc_root_hash:(mc_root ()) ~shard_proof:c.shard_proof
              ~workchain ~shard ~shard_root_hash:root_hash ~seqno ()))

let shard_err name r =
  match r with
  | Ok _ -> Alcotest.failf "%s: accepted" name
  | Error e -> Format.asprintf "%a" Shard.pp_error e

let test_shard_rejections () =
  let c = case "basechain_absent" in
  let sb = shardblk "basechain_absent" in
  let workchain = int_of_json (field "workchain" sb) in
  let shard = Int64.of_string ("0x" ^ to_str (field "shard" sb)) in
  let root_hash = unhex (to_str (field "rootHash" sb)) in
  let seqno = int_of_json (field "seqno" sb) in
  let v ?(wc = workchain) ?(sh = shard) ?(rh = root_hash) ?sq () =
    Shard.verify ~mc_root_hash:(mc_root ()) ~shard_proof:c.shard_proof
      ~workchain:wc ~shard:sh ~shard_root_hash:rh ?seqno:sq ()
  in
  (* A different shard block than the masterchain records. *)
  Alcotest.(check bool)
    "substituted block hash" true
    (let m = shard_err "hash" (v ~rh:(String.make 32 '\xcd') ()) in
     String.length m > 24 && String.sub m 0 24 = "shard block hash mismatc");
  Alcotest.(check bool)
    "substituted seqno" true
    (let m = shard_err "seqno" (v ~sq:(Int32.add seqno 1l) ()) in
     String.length m > 19 && String.sub m 0 19 = "shard seqno mismatc");
  (* A workchain the masterchain block says nothing about. *)
  Alcotest.(check bool)
    "unknown workchain" true
    (let m = shard_err "workchain" (v ~wc:7l ()) in
     String.length m > 30
     && String.sub m 0 30 = "the masterchain block records ");
  (* Aimed at a block the proof is not about. *)
  Alcotest.(check bool)
    "wrong masterchain block" true
    (let m =
       shard_err "block"
         (Shard.verify ~mc_root_hash:(String.make 32 '\xab')
            ~shard_proof:c.shard_proof ~workchain ~shard
            ~shard_root_hash:root_hash ())
     in
     String.length m > 8 && String.sub m 0 8 = "no proof")

(* Shard identifiers pack a prefix and its length into one word; the lowest
   set bit marks where the prefix ends. *)
let test_prefix_length () =
  List.iter
    (fun (shard, expected) ->
      Alcotest.(check int)
        (Printf.sprintf "%016Lx" shard)
        expected
        (Shard.prefix_length shard))
    [
      (0x8000000000000000L, 0);
      (0x4000000000000000L, 1);
      (0xc000000000000000L, 1);
      (0x2000000000000000L, 2);
      (0xa000000000000000L, 2);
      (0x0000000000000001L, 63);
    ]

(* A masterchain account has no shard link to prove: the block the account is
   proved against is the masterchain block itself. *)
let test_masterchain_has_no_shard_proof () =
  let c = case "elector" in
  Alcotest.(check string) "no shard proof is sent" "" c.shard_proof;
  Alcotest.(check string)
    "and the shard block is the masterchain block"
    (hex (mc_root ()))
    (hex c.shard_root)

(* --- the whole chain from a masterchain block -------------------------------- *)

let block_ref name =
  let sb = shardblk name in
  {
    Account.workchain = int_of_json (field "workchain" sb);
    shard = Int64.of_string ("0x" ^ to_str (field "shard" sb));
    seqno = int_of_json (field "seqno" sb);
    root_hash = unhex (to_str (field "rootHash" sb));
  }

(* The composed entry point: from a masterchain block alone, through the shard
   link, to the account. This is what a caller should reach for, because doing
   the two steps by hand invites skipping the first. *)
let test_whole_chain name expected () =
  let c = case name in
  match
    Account.verify_via_shard ~mc_root_hash:(mc_root ())
      ~shard_proof:c.shard_proof ~shardblk:(block_ref name) ~proof:c.proof
      ~state:c.state ~address:c.address
  with
  | Error e -> Alcotest.failf "%s: %a" name Account.pp_error e
  | Ok Account.Does_not_exist ->
      Alcotest.(check string) (name ^ ": absent") "absent" expected
  | Ok (Account.Exists _) ->
      Alcotest.(check string) (name ^ ": exists") "exists" expected

(* Substituting a shard block the masterchain does not name must fail before
   the account proof is even considered. *)
let test_whole_chain_rejects_shard () =
  let c = case "basechain_absent" in
  let bad =
    {
      (block_ref "basechain_absent") with
      Account.root_hash = String.make 32 '\x9e';
    }
  in
  match
    Account.verify_via_shard ~mc_root_hash:(mc_root ())
      ~shard_proof:c.shard_proof ~shardblk:bad ~proof:c.proof ~state:c.state
      ~address:c.address
  with
  | Ok _ -> Alcotest.fail "accepted a shard block the masterchain does not name"
  | Error (Account.Shard _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" Account.pp_error e

let () =
  Alcotest.run "proof"
    [
      ( "mainnet answers",
        List.map
          (fun n -> Alcotest.test_case n `Quick (test_verifies n))
          (Lazy.force case_names)
        @ [ Alcotest.test_case "absence is proved" `Quick test_absence ] );
      ( "tampering",
        [
          Alcotest.test_case "wrong block" `Quick test_wrong_block;
          Alcotest.test_case "swapped state" `Quick test_swapped_state;
          Alcotest.test_case "tampered state" `Quick test_tampered_state;
          Alcotest.test_case "state for an absent account" `Quick
            test_state_for_absent_account;
          Alcotest.test_case "address the proof does not cover" `Quick
            test_uncovered_address;
          Alcotest.test_case "wrong workchain" `Quick test_wrong_workchain;
        ] );
      ("merkle", [ Alcotest.test_case "accessors" `Quick test_merkle_accessors ]);
      ( "shard link",
        [
          Alcotest.test_case "the masterchain names the shard block" `Quick
            test_shard_link;
          Alcotest.test_case "rejections" `Quick test_shard_rejections;
          Alcotest.test_case "shard prefix lengths" `Quick test_prefix_length;
          Alcotest.test_case "masterchain needs no shard link" `Quick
            test_masterchain_has_no_shard_proof;
        ] );
      ( "whole chain",
        [
          Alcotest.test_case "elector" `Quick
            (test_whole_chain "elector" "exists");
          Alcotest.test_case "config" `Quick
            (test_whole_chain "config" "exists");
          Alcotest.test_case "basechain absent" `Quick
            (test_whole_chain "basechain_absent" "absent");
          Alcotest.test_case "rejects an unnamed shard block" `Quick
            test_whole_chain_rejects_shard;
        ] );
    ]
