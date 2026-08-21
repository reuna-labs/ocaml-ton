open Ton_cell

type error = Bad_magic of { expected : int32; got : int32 } | Malformed of string

let pp_error ppf = function
  | Bad_magic { expected; got } -> Format.fprintf ppf "expected magic %08lx, got %08lx" expected got
  | Malformed m -> Format.pp_print_string ppf m

let block_magic = 0x11ef55aal
let shard_state_magic = 0x9023afe2l
let ( let* ) = Result.bind

let read c f =
  match Slice.parse c f with
  | Ok v -> v
  | Error e -> Error (Malformed (Format.asprintf "%a" Slice.pp_error e))

let magic s expected =
  let got = Int64.to_int32 (Slice.load_uint s ~bits:32) in
  if got <> expected then Error (Bad_magic { expected; got }) else Ok ()

let block_state_update c =
  read c (fun s ->
      let* () = magic s block_magic in
      ignore (Slice.load_int s ~bits:32) (* global_id *);
      (* info, value_flow, state_update, extra -- the third reference. *)
      ignore (Slice.load_ref s);
      ignore (Slice.load_ref s);
      let su = Slice.load_ref s in
      Result.map_error (fun e -> Malformed (Format.asprintf "%a" Merkle.pp_error e)) (Merkle.update su))

type shard_info = { global_id : int32; workchain : int32; shard_prefix : int64; seqno : int32; gen_utime : int32 }

(* Reads the fixed prefix of a shard state. The bits and the references are
   separate streams, so [before_split] sitting between two references is not
   a complication -- the reference cursor simply advances independently. *)
let parse_shard_state s =
  let* () = magic s shard_state_magic in
  let global_id = Int64.to_int32 (Slice.load_int s ~bits:32) in
  (* shard_ident$00 shard_pfx_bits:(#<= 60) workchain_id:int32 shard_prefix:uint64 *)
  let tag = Slice.load_uint s ~bits:2 in
  if tag <> 0L then Error (Malformed "shard_ident must start with 00")
  else begin
    ignore (Slice.load_uint s ~bits:6) (* shard_pfx_bits *);
    let workchain = Int64.to_int32 (Slice.load_int s ~bits:32) in
    let shard_prefix = Slice.load_uint s ~bits:64 in
    let seqno = Int64.to_int32 (Slice.load_uint s ~bits:32) in
    ignore (Slice.load_uint s ~bits:32) (* vert_seq_no *);
    let gen_utime = Int64.to_int32 (Slice.load_uint s ~bits:32) in
    ignore (Slice.load_uint s ~bits:64) (* gen_lt *);
    ignore (Slice.load_uint s ~bits:32) (* min_ref_mc_seqno *);
    Ok { global_id; workchain; shard_prefix; seqno; gen_utime }
  end

let shard_state_info c = read c (fun s -> parse_shard_state s)

let shard_state_accounts c =
  read c (fun s ->
      let* _ = parse_shard_state s in
      ignore (Slice.load_ref s) (* out_msg_queue_info *);
      ignore (Slice.load_bit s) (* before_split *);
      Ok (Slice.load_ref s))
