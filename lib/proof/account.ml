open Ton_cell

type outcome = Exists of Cell.t | Does_not_exist

type error =
  | Boc of Boc.error
  | Merkle of Merkle.error
  | State of State.error
  | Dict of Slice.error
  | No_block_proof of { want : string }
  | No_state_proof of { want : string }
  | Elided of string
  | Hash_mismatch of { want : string; got : string }
  | Unexpected_state of string
  | Wrong_workchain of { want : int32; got : int32 }

let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let pp_error ppf = function
  | Boc e -> Boc.pp_error ppf e
  | Merkle e -> Merkle.pp_error ppf e
  | State e -> State.pp_error ppf e
  | Dict e -> Slice.pp_error ppf e
  | No_block_proof { want } -> Format.fprintf ppf "no proof of block %s among the roots" want
  | No_state_proof { want } -> Format.fprintf ppf "no proof of state %s among the roots" want
  | Elided what -> Format.fprintf ppf "the proof does not cover %s" what
  | Hash_mismatch { want; got } -> Format.fprintf ppf "hash mismatch: expected %s, got %s" want got
  | Unexpected_state m -> Format.pp_print_string ppf m
  | Wrong_workchain { want; got } ->
      Format.fprintf ppf "proof is for workchain %ld, address is in %ld" got want

let ( let* ) = Result.bind
let account_key_bits = 256

(* depth_balance$_ split_depth:(#<= 30) balance:CurrencyCollection *)
let read_depth_balance s =
  ignore (Slice.load_uint s ~bits:5);
  ignore (Ton_tlb.Currency.load s)

(* account_descr$_ account:^Account last_trans_hash:bits256 last_trans_lt:uint64.
   Only the reference matters here: in a proof it is usually a pruned branch,
   and a pruned branch reports the hash of what it replaced, which is exactly
   what has to be compared against. *)
let read_shard_account s = Slice.load_ref s

let find_proof_for roots hash =
  List.find_map
    (fun c ->
      match Merkle.proof c with Ok p when String.equal p.Merkle.hash hash -> Some p | _ -> None)
    roots

let verify ~block_root_hash ~proof ~state ~address =
  let* roots = Result.map_error (fun e -> Boc e) (Boc.deserialize proof) in
  (* 1. A proof of the block we already trust. *)
  let* block_proof =
    match find_proof_for roots block_root_hash with
    | Some p -> Ok p
    | None -> Error (No_block_proof { want = hex block_root_hash })
  in
  (* 2. The state that block produced. *)
  let* update = Result.map_error (fun e -> State e) (State.block_state_update block_proof.Merkle.root) in
  let state_hash = update.Merkle.new_hash in
  (* 3. A proof of that state. *)
  let* state_proof =
    match find_proof_for roots state_hash with
    | Some p -> Ok p
    | None -> Error (No_state_proof { want = hex state_hash })
  in
  let* info = Result.map_error (fun e -> State e) (State.shard_state_info state_proof.Merkle.root) in
  let want_wc = Int32.of_int address.Ton_address.workchain in
  let* () =
    if info.State.workchain <> want_wc then
      Error (Wrong_workchain { want = want_wc; got = info.State.workchain })
    else Ok ()
  in
  (* 4. The account dictionary, and the key's path through it. *)
  let* accounts =
    Result.map_error (fun e -> State e) (State.shard_state_accounts state_proof.Merkle.root)
  in
  if Cell.is_exotic accounts then Error (Elided "the account dictionary")
  else
    let* root =
      match
        Slice.parse accounts (fun s ->
            if not (Slice.load_bit s) then None (* ahme_empty *) else Some (Slice.load_ref s))
      with
      | Ok v -> Ok v
      | Error e -> Error (Dict e)
    in
    let key = Z.of_string_base 16 (hex address.Ton_address.hash) in
    let* found =
      match root with
      | None -> Ok Ton_tlb.Dict.Absent (* the shard genuinely holds no accounts *)
      | Some root ->
          Result.map_error
            (fun e -> Dict e)
            (Ton_tlb.Dict.lookup_aug root ~key_bits:account_key_bits ~key ~extra:read_depth_balance
               ~value:read_shard_account)
    in
    match found with
    | Ton_tlb.Dict.Elided -> Error (Elided "the account's position in the dictionary")
    | Ton_tlb.Dict.Absent ->
        if state = "" then Ok Does_not_exist
        else Error (Unexpected_state "the proof says the account is absent but a state was supplied")
    | Ton_tlb.Dict.Found account_ref ->
        if state = "" then
          Error (Unexpected_state "the proof places the account in the shard but no state was supplied")
        else
          let* cell = Result.map_error (fun e -> Boc e) (Boc.deserialize_root state) in
          (* The committed hash is level 0, which is what a pruned branch
             impersonates -- so this works whether the proof carried the
             account itself or only a stand-in for it. *)
          let want = Cell.hash account_ref and got = Cell.hash cell in
          if String.equal want got then Ok (Exists cell)
          else Error (Hash_mismatch { want = hex want; got = hex got })
