open Ton_cell

type error =
  | State of State.error
  | Not_masterchain
  | Not_a_key_block
  | Elided of string
  | Malformed of string

let pp_error ppf = function
  | State e -> State.pp_error ppf e
  | Not_masterchain -> Format.fprintf ppf "the proved state is not a masterchain state"
  | Not_a_key_block -> Format.fprintf ppf "the block is not a key block and carries no configuration"
  | Elided what -> Format.fprintf ppf "the proof does not cover %s" what
  | Malformed m -> Format.pp_print_string ppf m

let ( let* ) = Result.bind
let mc_state_extra_magic = 0xcc26
let current_validators = 34l
let next_validators = 36l

let of_state state_root =
  let* custom = Result.map_error (fun e -> State e) (State.shard_state_custom state_root) in
  let* extra = match custom with Some c -> Ok c | None -> Error Not_masterchain in
  if Cell.is_exotic extra then Error (Elided "the masterchain state extra")
  else
    match
      Slice.parse extra (fun s ->
          let magic = Int64.to_int (Slice.load_uint s ~bits:16) in
          if magic <> mc_state_extra_magic then
            Slice.fail (Slice.Message (Printf.sprintf "expected McStateExtra magic cc26, got %04x" magic));
          (* shard_hashes is a HashmapE, so it costs a bit and possibly a
             reference before the configuration is reached. *)
          ignore (Slice.load_maybe_ref s);
          ignore (Slice.load_bytes s 32) (* config_addr *);
          Slice.load_ref s)
    with
    | Ok c -> Ok c
    | Error e -> Error (Malformed (Format.asprintf "%a" Slice.pp_error e))

let block_magic = 0x11ef55aal
let mc_block_extra_magic = 0xcca5

(* A TL-B constructor written without an explicit tag still carries one: a
   32-bit CRC-32 of its own definition, the same rule TL uses for constructor
   identifiers. BlockExtra is one of those, and forgetting the tag shifts
   every field after it. The test suite recomputes this from the schema text
   rather than taking it on trust. *)
let block_extra_tag = 0x4a33f6fdl

let block_extra_definition =
  "block_extra in_msg_descr:^InMsgDescr out_msg_descr:^OutMsgDescr \
   account_blocks:^ShardAccountBlocks rand_seed:bits256 created_by:bits256 \
   custom:(Maybe ^McBlockExtra) = BlockExtra"

let of_key_block block_root =
  if Cell.is_exotic block_root then Error (Elided "the key block")
  else
    match
      Slice.parse block_root (fun s ->
          let magic = Int64.to_int32 (Slice.load_uint s ~bits:32) in
          if magic <> block_magic then
            Slice.fail (Slice.Message (Printf.sprintf "expected Block magic 11ef55aa, got %08lx" magic));
          ignore (Slice.load_int s ~bits:32) (* global_id *);
          ignore (Slice.load_ref s) (* info *);
          ignore (Slice.load_ref s) (* value_flow *);
          ignore (Slice.load_ref s) (* state_update, pruned in this kind of proof *);
          Slice.load_ref s)
    with
    | Error e -> Error (Malformed (Format.asprintf "%a" Slice.pp_error e))
    | Ok extra ->
        if Cell.is_exotic extra then Error (Elided "the block extra")
        else (
          match
            Slice.parse extra (fun s ->
                let tag = Int64.to_int32 (Slice.load_uint s ~bits:32) in
                if tag <> block_extra_tag then
                  Slice.fail
                    (Slice.Message (Printf.sprintf "expected BlockExtra tag %08lx, got %08lx" block_extra_tag tag));
                ignore (Slice.load_ref s) (* in_msg_descr *);
                ignore (Slice.load_ref s) (* out_msg_descr *);
                ignore (Slice.load_ref s) (* account_blocks *);
                ignore (Slice.load_bytes s 32) (* rand_seed *);
                ignore (Slice.load_bytes s 32) (* created_by *);
                Slice.load_maybe_ref s)
          with
          | Error e -> Error (Malformed (Format.asprintf "%a" Slice.pp_error e))
          | Ok None -> Error Not_masterchain
          | Ok (Some custom) ->
              if Cell.is_exotic custom then Error (Elided "the masterchain block extra")
              else (
                match
                  Slice.parse custom (fun s ->
                      let magic = Int64.to_int (Slice.load_uint s ~bits:16) in
                      if magic <> mc_block_extra_magic then
                        Slice.fail
                          (Slice.Message
                             (Printf.sprintf "expected McBlockExtra magic cca5, got %04x" magic));
                      let key_block = Slice.load_bit s in
                      ignore (Slice.load_maybe_ref s) (* shard_hashes *);
                      (* shard_fees is augmented, so it carries an aggregate
                         whether or not the map itself is present. *)
                      ignore (Slice.load_maybe_ref s);
                      ignore (Ton_tlb.Currency.load s);
                      ignore (Ton_tlb.Currency.load s);
                      ignore (Slice.load_ref s) (* prev_blk_signatures and friends *);
                      if not key_block then None
                      else begin
                        ignore (Slice.load_bytes s 32) (* config_addr *);
                        Some (Slice.load_ref s)
                      end)
                with
                | Error e -> Error (Malformed (Format.asprintf "%a" Slice.pp_error e))
                | Ok None -> Error Not_a_key_block
                | Ok (Some c) -> Ok c))

let param root n =
  if Cell.is_exotic root then Error (Elided "the configuration dictionary")
  else
    let key = Z.logand (Z.of_int32 n) (Z.of_string "0xffffffff") in
    match
      Ton_tlb.Dict.lookup root ~key_bits:32 ~key ~value:(fun s -> Slice.load_ref s)
    with
    | Error e -> Error (Malformed (Format.asprintf "%a" Slice.pp_error e))
    | Ok Ton_tlb.Dict.Elided -> Error (Elided (Printf.sprintf "configuration parameter %ld" n))
    | Ok Ton_tlb.Dict.Absent -> Ok None
    | Ok (Ton_tlb.Dict.Found c) -> Ok (Some c)
