(** Tying a shard block back to the masterchain.

    An account outside the masterchain is proved against the {e shard} block
    that contains it. That leaves a gap: the liteserver also chose which shard
    block to name. Closing it means proving that the shard block appears in
    the masterchain block's shard hashes.

    {v
    masterchain_state_extra#cc26 shard_hashes:ShardHashes … = McStateExtra;
    _ (HashmapE 32 ^(BinTree ShardDescr)) = ShardHashes;
    bt_leaf$0 leaf:X = BinTree X;
    bt_fork$1 left:^(BinTree X) right:^(BinTree X) = BinTree X;
    shard_descr#b seq_no:uint32 reg_mc_seqno:uint32 start_lt:uint64
      end_lt:uint64 root_hash:bits256 file_hash:bits256 … = ShardDescr;
    v}

    A shard identifier packs a prefix and its length into one 64-bit word: the
    lowest set bit marks the end of the prefix, so [0x8000…0] is an unsplit
    workchain and each split appends a bit. The binary tree is indexed by
    exactly those prefix bits. *)

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

val pp_error : Format.formatter -> error -> unit

val prefix_length : int64 -> int
(** How many bits of a shard identifier are prefix. [0x8000000000000000] is
    an unsplit workchain and has none. *)

val find :
  mc_root_hash:string -> shard_proof:string -> workchain:int32 -> shard:int64 ->
  (descr, error) result
(** The shard descriptor the masterchain block records for a shard. *)

val verify :
  mc_root_hash:string -> shard_proof:string -> workchain:int32 -> shard:int64 ->
  shard_root_hash:string -> ?seqno:int32 -> unit -> (unit, error) result
(** Check that the masterchain block really names this shard block, so that
    an account proof rooted at it can be trusted. *)
