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

let chain =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/block-proof.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "block-proof.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"
let to_list = function `List l -> l | _ -> Alcotest.fail "expected a list"
let to_i32 = function `Int n -> Int32.of_int n | `Intlit s -> Int32.of_string s | _ -> Alcotest.fail "expected an int"
let b64 s = match Base64.decode s with Ok v -> v | Error (`Msg m) -> Alcotest.fail m

let block_of j =
  let j = to_obj j in
  { Block_proof.seqno = to_i32 (field "seqno" j);
    root_hash = unhex (to_str (field "rootHash" j));
    file_hash = unhex (to_str (field "fileHash" j)) }

let steps = lazy (List.map to_obj (to_list (field "steps" (Lazy.force chain))))

let signatures_of step =
  let s = to_obj (field "signatures" step) in
  List.map
    (fun j ->
      let j = to_obj j in
      { Block_proof.who = unhex (to_str (field "who" j)); signature = b64 (to_str (field "signature" j)) })
    (to_list (field "signatures" s))

(* --- the anchor ------------------------------------------------------------- *)

(* Everything rests on a block published out of band in TON's global config.
   The chain only means anything if the first link starts there. *)
let test_anchor () =
  let init = block_of (field "initBlock" (Lazy.force chain)) in
  let first = List.hd (Lazy.force steps) in
  let from = block_of (field "from" first) in
  Alcotest.(check string) "the first link starts at the configured init block" (hex init.Block_proof.root_hash)
    (hex from.Block_proof.root_hash);
  Alcotest.(check int32) "and at its seqno" init.Block_proof.seqno from.Block_proof.seqno

(* --- what gets signed --------------------------------------------------------- *)

(* Validators sign a TL-serialised ton.blockId, which commits to the block's
   root hash and to the hash of its serialised form -- so a signature binds
   both, not just the cell tree. *)
let test_to_sign () =
  let b =
    { Block_proof.seqno = 1l; root_hash = String.make 32 '\x11'; file_hash = String.make 32 '\x22' }
  in
  let m = Block_proof.to_sign b in
  Alcotest.(check int) "constructor plus two hashes" 68 (String.length m);
  Alcotest.(check string) "ton.blockId, little-endian" "706e0bc5" (hex (String.sub m 0 4));
  Alcotest.(check string) "then the root hash" (hex b.Block_proof.root_hash) (hex (String.sub m 4 32));
  Alcotest.(check string) "then the file hash" (hex b.Block_proof.file_hash) (hex (String.sub m 36 32))

(* A TL-B constructor written without an explicit tag still carries one: the
   CRC-32 of its own definition, exactly the rule TL uses for constructor
   identifiers. BlockExtra is such a constructor, and missing its tag shifts
   every field after it -- so the constant is recomputed here from the schema
   text rather than trusted. *)
let test_implicit_tlb_tag () =
  Alcotest.(check string) "BlockExtra's implicit tag"
    (Printf.sprintf "%08lx" (Ton_tl.Tl.Crc.constructor_id Config.block_extra_definition))
    (Printf.sprintf "%08lx" Config.block_extra_tag)

(* --- validator sets ----------------------------------------------------------- *)

let sets_of step =
  let from = block_of (field "from" step) in
  match
    Block_proof.validator_set_of_config_proof ~from_root_hash:from.Block_proof.root_hash
      ~config_proof:(b64 (to_str (field "configProof" step)))
  with
  | Ok s -> s
  | Error e -> Alcotest.failf "%a" Block_proof.pp_error e

(* Only the first [main] validators sign a masterchain block. Measuring
   against the whole set instead puts the threshold permanently out of reach,
   which is a failure that looks like "the signatures are wrong". *)
let test_main_subset () =
  let sets = sets_of (List.hd (Lazy.force steps)) in
  List.iter
    (fun (v : Validators.t) ->
      Alcotest.(check int) "the signing subset is main-sized" v.Validators.main
        (List.length (Validators.main_list v));
      Alcotest.(check bool) "which is no larger than the whole set" true
        (v.Validators.main <= v.Validators.total);
      Alcotest.(check bool) "and carries less weight than the whole set" true
        (Int64.compare (Validators.main_weight v) v.Validators.total_weight <= 0);
      Alcotest.(check bool) "a member outside it does not count" true
        (match List.nth_opt v.Validators.list v.Validators.main with
        | None -> true (* the set is entirely main *)
        | Some outside -> Validators.find v ~short_id:outside.Validators.short_id = None))
    sets

(* The set comes out of the source block itself, which is why the link can be
   followed at all: a key block names who may sign the next one. *)
let test_validator_sets () =
  let sets = sets_of (List.hd (Lazy.force steps)) in
  Alcotest.(check bool) "at least one set" true (sets <> []);
  List.iter
    (fun (v : Validators.t) ->
      Alcotest.(check bool) "has members" true (v.Validators.list <> []);
      Alcotest.(check int) "the count matches the list" (List.length v.Validators.list) v.Validators.total;
      let summed = List.fold_left (fun a (m : Validators.validator) -> Int64.add a m.Validators.weight) 0L v.Validators.list in
      Alcotest.(check int64) "weights sum to the stated total" v.Validators.total_weight summed;
      List.iter
        (fun (m : Validators.validator) ->
          Alcotest.(check int) "key is 32 bytes" 32 (String.length m.Validators.public_key);
          Alcotest.(check int) "short id is 32 bytes" 32 (String.length m.Validators.short_id);
          Alcotest.(check bool) "weight is positive" true (Int64.compare m.Validators.weight 0L > 0))
        v.Validators.list)
    sets

(* --- following the chain ------------------------------------------------------ *)

let follow step =
  let from = block_of (field "from" step) in
  let dest = block_of (field "to" step) in
  Block_proof.verify_forward ~from_root_hash:from.Block_proof.root_hash
    ~config_proof:(b64 (to_str (field "configProof" step)))
    ~dest_proof:(b64 (to_str (field "destProof" step)))
    ~dest ~signatures:(signatures_of step)

(* Each link is followed from a block already trusted to the next one, and is
   accepted only because validators holding more than two thirds of the
   weight signed it. *)
let test_follow_each () =
  List.iteri
    (fun i step ->
      match follow step with
      | Error e -> Alcotest.failf "step %d: %a" i Block_proof.pp_error e
      | Ok o ->
          let at f = Printf.sprintf "step %d: %s" i f in
          Alcotest.(check bool) (at "more than two thirds signed") true
            (Int64.compare (Int64.mul o.Block_proof.signed_weight 3L)
               (Int64.mul o.Block_proof.total_weight 2L)
            > 0);
          Alcotest.(check bool) (at "most offered signatures verified") true
            (o.Block_proof.accepted * 10 >= o.Block_proof.offered * 9);
          Alcotest.(check int32) (at "arrives at the stated block")
            (block_of (field "to" step)).Block_proof.seqno o.Block_proof.next.Block_proof.seqno)
    (Lazy.force steps)

(* Walking the chain: each link's destination is the next link's source, so
   trust propagates from the anchor to the far end. *)
let test_chain_is_connected () =
  let steps = Lazy.force steps in
  let rec go trusted = function
    | [] -> trusted
    | step :: rest ->
        let from = block_of (field "from" step) in
        Alcotest.(check string) "the link starts where the last one ended" (hex trusted)
          (hex from.Block_proof.root_hash);
        (match follow step with
        | Error e -> Alcotest.failf "%a" Block_proof.pp_error e
        | Ok o -> go o.Block_proof.next.Block_proof.root_hash rest)
  in
  let init = block_of (field "initBlock" (Lazy.force chain)) in
  let final = go init.Block_proof.root_hash steps in
  let last = List.nth steps (List.length steps - 1) in
  Alcotest.(check string) "ends at the last block in the chain"
    (hex (block_of (field "to" last)).Block_proof.root_hash)
    (hex final)

(* --- what must be rejected ----------------------------------------------------- *)

let err name r =
  match r with
  | Ok _ -> Alcotest.failf "%s: accepted" name
  | Error e -> Format.asprintf "%a" Block_proof.pp_error e

let step0 () = List.hd (Lazy.force steps)

(* Signatures are over the destination's root and file hashes, so changing
   either must invalidate every one of them. *)
let test_substituted_destination () =
  let step = step0 () in
  let from = block_of (field "from" step) in
  let dest = block_of (field "to" step) in
  let attempt d =
    Block_proof.verify_forward ~from_root_hash:from.Block_proof.root_hash
      ~config_proof:(b64 (to_str (field "configProof" step)))
      ~dest_proof:(b64 (to_str (field "destProof" step)))
      ~dest:d ~signatures:(signatures_of step)
  in
  let m = err "root hash" (attempt { dest with Block_proof.root_hash = String.make 32 '\xaa' }) in
  Alcotest.(check bool) "destination proof no longer matches" true
    (String.length m > 14 && String.sub m 0 14 = "the destinatio");
  (* Keeping the root hash but changing the file hash gets past the proof and
     must then fail on the signatures. *)
  let m = err "file hash" (attempt { dest with Block_proof.file_hash = String.make 32 '\xbb' }) in
  Alcotest.(check bool) "no weight gathers" true
    (String.length m > 10 && String.sub m 0 10 = "signatures")

(* A link is only as good as the block it starts from. *)
let test_wrong_source () =
  let step = step0 () in
  let dest = block_of (field "to" step) in
  let m =
    err "wrong source"
      (Block_proof.verify_forward ~from_root_hash:(String.make 32 '\xcc')
         ~config_proof:(b64 (to_str (field "configProof" step)))
         ~dest_proof:(b64 (to_str (field "destProof" step)))
         ~dest ~signatures:(signatures_of step))
  in
  Alcotest.(check bool) "no proof of that block" true (String.length m > 8 && String.sub m 0 8 = "no proof")

let test_no_signatures () =
  let step = step0 () in
  let from = block_of (field "from" step) in
  let m =
    err "none"
      (Block_proof.verify_forward ~from_root_hash:from.Block_proof.root_hash
         ~config_proof:(b64 (to_str (field "configProof" step)))
         ~dest_proof:(b64 (to_str (field "destProof" step)))
         ~dest:(block_of (field "to" step)) ~signatures:[])
  in
  Alcotest.(check bool) "carries no weight" true
    (String.length m > 10 && String.sub m 0 10 = "signatures")

(* Dropping signatures until the threshold is missed must flip the answer.
   This is the check that the two-thirds rule is actually enforced rather
   than assumed. *)
let test_threshold_is_enforced () =
  let step = step0 () in
  let from = block_of (field "from" step) in
  let all = signatures_of step in
  let attempt sigs =
    Block_proof.verify_forward ~from_root_hash:from.Block_proof.root_hash
      ~config_proof:(b64 (to_str (field "configProof" step)))
      ~dest_proof:(b64 (to_str (field "destProof" step)))
      ~dest:(block_of (field "to" step)) ~signatures:sigs
  in
  let take n = List.filteri (fun i _ -> i < n) all in
  let n = List.length all in
  Alcotest.(check bool) "all of them is enough" true (Result.is_ok (attempt all));
  Alcotest.(check bool) "half of them is not" false (Result.is_ok (attempt (take (n / 2))));
  Alcotest.(check bool) "one is not" false (Result.is_ok (attempt (take 1)))

(* A signature that verifies under the wrong key must not count. *)
let test_forged_signature () =
  let step = step0 () in
  let from = block_of (field "from" step) in
  let all = signatures_of step in
  let corrupted =
    List.map
      (fun (s : Block_proof.signature) ->
        let b = Bytes.of_string s.Block_proof.signature in
        Bytes.set b 0 (Char.chr (Char.code (Bytes.get b 0) lxor 0xff));
        { s with Block_proof.signature = Bytes.to_string b })
      all
  in
  let m =
    err "forged"
      (Block_proof.verify_forward ~from_root_hash:from.Block_proof.root_hash
         ~config_proof:(b64 (to_str (field "configProof" step)))
         ~dest_proof:(b64 (to_str (field "destProof" step)))
         ~dest:(block_of (field "to" step)) ~signatures:corrupted)
  in
  Alcotest.(check bool) "none of them counts" true
    (String.length m > 10 && String.sub m 0 10 = "signatures")

let () =
  Alcotest.run "block proof"
    [ ( "anchor",
        [ Alcotest.test_case "starts at the configured init block" `Quick test_anchor;
          Alcotest.test_case "what a validator signs" `Quick test_to_sign;
          Alcotest.test_case "implicit TL-B constructor tags" `Quick test_implicit_tlb_tag ] );
      ( "validator sets",
        [ Alcotest.test_case "read from the source block" `Quick test_validator_sets;
          Alcotest.test_case "only the main subset signs" `Quick test_main_subset ] );
      ( "following the chain",
        [ Alcotest.test_case "each link" `Quick test_follow_each;
          Alcotest.test_case "the chain is connected" `Quick test_chain_is_connected ] );
      ( "rejections",
        [ Alcotest.test_case "substituted destination" `Quick test_substituted_destination;
          Alcotest.test_case "wrong source block" `Quick test_wrong_source;
          Alcotest.test_case "no signatures" `Quick test_no_signatures;
          Alcotest.test_case "the threshold is enforced" `Quick test_threshold_is_enforced;
          Alcotest.test_case "forged signatures" `Quick test_forged_signature ] )
    ]
