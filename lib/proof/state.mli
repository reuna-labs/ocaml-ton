(** Navigating a block and a shard state far enough to reach an account.

    Only the fields on the path matter; everything else in these structures
    is normally a pruned branch in a proof anyway. *)

open Ton_cell

type error = Bad_magic of { expected : int32; got : int32 } | Malformed of string

val pp_error : Format.formatter -> error -> unit

val block_state_update : Cell.t -> (Merkle.update, error) result
(** [block#11ef55aa … state_update:^(MERKLE_UPDATE ShardState) …]. Its
    [new_hash] is the state the block produced, which is what an account
    proof is rooted at. *)

type shard_info = { global_id : int32; workchain : int32; shard_prefix : int64; seqno : int32; gen_utime : int32 }

val shard_state_info : Cell.t -> (shard_info, error) result
val shard_state_accounts : Cell.t -> (Cell.t, error) result
(** The [accounts:^ShardAccounts] reference — a cell holding a
    [HashmapAugE 256 ShardAccount DepthBalanceInfo]. *)

val shard_state_custom : Cell.t -> (Cell.t option, error) result
(** The [custom:(Maybe ^McStateExtra)] reference, present only on the
    masterchain. It is where the shard hashes live, and hence the only place
    a shard block can be tied back to a masterchain block. *)
