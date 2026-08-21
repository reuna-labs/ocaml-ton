(** Verifying an account against a block.

    A liteserver answering [getAccountState] returns the account together
    with Merkle proofs. Without checking them the client is simply trusting
    the server, which for something running inside an attested unikernel
    defeats the point.

    Verification follows a chain, and every link has to hold:

    {v
    block root hash  ──(block proof)──▶  Block
                                          │ state_update.new_hash
                                          ▼
                     ──(state proof)──▶  ShardStateUnsplit
                                          │ accounts
                                          ▼
                                        ShardAccounts, keyed by address
                                          │
                                          ▼
                                        the account cell's hash
    v}

    Absence is proved the same way and matters just as much: a server that
    simply omitted the relevant subtree would otherwise look identical to one
    showing that an account really is not there. *)

open Ton_cell

type outcome =
  | Exists of Cell.t
      (** The account cell, having matched the hash the proof commits to. *)
  | Does_not_exist
      (** The proof covers where the account would be, and it is not there. *)

type block_ref = { workchain : int32; shard : int64; seqno : int32; root_hash : string }
(** A block as a liteserver names it — the fields of [tonNode.blockIdExt]
    that matter for verification. *)

type error =
  | Boc of Boc.error
  | Shard of Shard.error
  | Merkle of Merkle.error
  | State of State.error
  | Dict of Slice.error
  | No_block_proof of { want : string }
  | No_state_proof of { want : string }
  | Elided of string
  | Hash_mismatch of { want : string; got : string }
  | Unexpected_state of string
  | Wrong_workchain of { want : int32; got : int32 }

val pp_error : Format.formatter -> error -> unit

val verify :
  block_root_hash:string -> proof:string -> state:string -> address:Ton_address.t ->
  (outcome, error) result
(** [block_root_hash] must be a block the caller already trusts — for a
    masterchain account, the block the query names; for a shard account, the
    shard block, which needs its own proof against the masterchain first.

    [proof] and [state] are the Bags of Cells the liteserver returned; [state]
    is empty when it claims the account does not exist. *)

val verify_via_shard :
  mc_root_hash:string -> shard_proof:string -> shardblk:block_ref -> proof:string ->
  state:string -> address:Ton_address.t -> (outcome, error) result
(** Verify an account given only a trusted {e masterchain} block.

    This is the whole chain, and the reason to prefer it over {!verify}:
    the shard block an account is proved against is itself chosen by the
    server, so it has to be tied back to the masterchain before the account
    proof means anything. When [shardblk] is the masterchain block there is
    nothing to tie and [shard_proof] is expected to be empty. *)
