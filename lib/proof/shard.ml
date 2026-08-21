open Ton_cell

type descr = { seqno : int32; root_hash : string; file_hash : string; start_lt : int64; end_lt : int64 }

type error =
  | Boc of Boc.error
  | Merkle of Merkle.error
  | State of State.error
  | Dict of Slice.error
  | No_block_proof of { want : string }
  | No_state_proof of { want : string }
  | Not_masterchain
  | Elided of string
  | No_such_shard of { workchain : int32; shard : int64 }
  | Hash_mismatch of { want : string; got : string }
  | Seqno_mismatch of { want : int32; got : int32 }

let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let pp_error ppf = function
  | Boc e -> Boc.pp_error ppf e
  | Merkle e -> Merkle.pp_error ppf e
  | State e -> State.pp_error ppf e
  | Dict e -> Slice.pp_error ppf e
  | No_block_proof { want } -> Format.fprintf ppf "no proof of block %s among the roots" want
  | No_state_proof { want } -> Format.fprintf ppf "no proof of state %s among the roots" want
  | Not_masterchain -> Format.fprintf ppf "the proved state is not a masterchain state"
  | Elided what -> Format.fprintf ppf "the proof does not cover %s" what
  | No_such_shard { workchain; shard } ->
      Format.fprintf ppf "the masterchain block records no shard %ld:%016Lx" workchain shard
  | Hash_mismatch { want; got } ->
      Format.fprintf ppf "shard block hash mismatch: masterchain records %s, was given %s" want got
  | Seqno_mismatch { want; got } ->
      Format.fprintf ppf "shard seqno mismatch: masterchain records %ld, was given %ld" want got

let ( let* ) = Result.bind
let mc_state_extra_magic = 0xcc26

(* The lowest set bit terminates the prefix, so the number of prefix bits is
   how far that bit sits from the bottom, counted down from 63. *)
let prefix_length shard =
  if Int64.equal shard 0L then 0
  else
    let rec ctz i = if Int64.logand (Int64.shift_right_logical shard i) 1L = 1L then i else ctz (i + 1) in
    63 - ctz 0

let shard_bit shard i = Int64.logand (Int64.shift_right_logical shard (63 - i)) 1L = 1L

(* shard_descr#b and shard_descr_new#a agree on everything up to file_hash,
   differing only in whether the trailing currency fields are inline or in a
   reference, so the fields that identify a block sit at the same offsets. *)
let read_descr s =
  let tag = Int64.to_int (Slice.load_uint s ~bits:4) in
  if tag <> 0xb && tag <> 0xa then Slice.fail (Slice.Message (Printf.sprintf "unknown shard_descr tag %x" tag));
  let seqno = Int64.to_int32 (Slice.load_uint s ~bits:32) in
  ignore (Slice.load_uint s ~bits:32) (* reg_mc_seqno *);
  let start_lt = Slice.load_uint s ~bits:64 in
  let end_lt = Slice.load_uint s ~bits:64 in
  let root_hash = Slice.load_bytes s 32 in
  let file_hash = Slice.load_bytes s 32 in
  { seqno; root_hash; file_hash; start_lt; end_lt }

exception Pruned of string

(* Descends the binary tree along the shard's prefix bits. A leaf reached
   early describes a coarser shard, which is not the one asked about. *)
let rec walk_bintree cell ~depth ~target_len ~shard =
  if Cell.is_exotic cell then raise (Pruned "the shard tree");
  match
    Slice.parse cell (fun s ->
        if not (Slice.load_bit s) then `Leaf (read_descr s)
        else
          let left = Slice.load_ref s in
          let right = Slice.load_ref s in
          `Fork (left, right))
  with
  | Error e -> Error (Dict e)
  | Ok (`Leaf d) -> if depth = target_len then Ok (Some d) else Ok None
  | Ok (`Fork (left, right)) ->
      if depth >= target_len then Ok None
      else
        walk_bintree (if shard_bit shard depth then right else left) ~depth:(depth + 1) ~target_len ~shard

let find_proof_for roots hash =
  List.find_map
    (fun c -> match Merkle.proof c with Ok p when String.equal p.Merkle.hash hash -> Some p | _ -> None)
    roots

let find ~mc_root_hash ~shard_proof ~workchain ~shard =
  let* roots = Result.map_error (fun e -> Boc e) (Boc.deserialize shard_proof) in
  let* block_proof =
    match find_proof_for roots mc_root_hash with
    | Some p -> Ok p
    | None -> Error (No_block_proof { want = hex mc_root_hash })
  in
  let* update = Result.map_error (fun e -> State e) (State.block_state_update block_proof.Merkle.root) in
  let* state_proof =
    match find_proof_for roots update.Merkle.new_hash with
    | Some p -> Ok p
    | None -> Error (No_state_proof { want = hex update.Merkle.new_hash })
  in
  let* custom = Result.map_error (fun e -> State e) (State.shard_state_custom state_proof.Merkle.root) in
  let* extra = match custom with Some c -> Ok c | None -> Error Not_masterchain in
  if Cell.is_exotic extra then Error (Elided "the masterchain state extra")
  else
    (* McStateExtra begins with its magic and then the shard hashes. *)
    let* hashes =
      match
        Slice.parse extra (fun s ->
            let magic = Int64.to_int (Slice.load_uint s ~bits:16) in
            if magic <> mc_state_extra_magic then
              Slice.fail (Slice.Message (Printf.sprintf "expected McStateExtra magic cc26, got %04x" magic));
            Slice.load_maybe_ref s)
      with
      | Ok v -> Ok v
      | Error e -> Error (Dict e)
    in
    let* hashes = match hashes with Some c -> Ok c | None -> Error (No_such_shard { workchain; shard }) in
    (* ShardHashes is keyed by workchain, and each value is a reference to the
       shard's binary tree. *)
    let key = Z.logand (Z.of_int32 workchain) (Z.of_string "0xffffffff") in
    let* found =
      Result.map_error (fun e -> Dict e)
        (Ton_tlb.Dict.lookup hashes ~key_bits:32 ~key ~value:(fun s -> Slice.load_ref s))
    in
    match found with
    | Ton_tlb.Dict.Elided -> Error (Elided "the workchain's shard tree")
    | Ton_tlb.Dict.Absent -> Error (No_such_shard { workchain; shard })
    | Ton_tlb.Dict.Found tree -> (
        let target_len = prefix_length shard in
        match walk_bintree tree ~depth:0 ~target_len ~shard with
        | exception Pruned what -> Error (Elided what)
        | Error _ as e -> e
        | Ok None -> Error (No_such_shard { workchain; shard })
        | Ok (Some d) -> Ok d)

let verify ~mc_root_hash ~shard_proof ~workchain ~shard ~shard_root_hash ?seqno () =
  let* d = find ~mc_root_hash ~shard_proof ~workchain ~shard in
  let* () =
    if String.equal d.root_hash shard_root_hash then Ok ()
    else Error (Hash_mismatch { want = hex d.root_hash; got = hex shard_root_hash })
  in
  match seqno with
  | Some want when want <> d.seqno -> Error (Seqno_mismatch { want = d.seqno; got = want })
  | _ -> Ok ()
