(** Block proof chains.

    Everything else in this library verifies against a masterchain block root
    that the caller already trusts. This is where that root comes from: a
    chain of links walked from an anchor published out of band — the
    [init_block] in TON's global configuration — to a recent block.

    A forward link carries three things: a proof of the destination block, a
    proof of the {e source} block's state containing the validator set, and
    the validators' signatures over the destination. Following it means
    checking that more than two thirds of the set the source block appoints
    signed the destination.

    {v
    liteServer.blockLinkForward to_key_block:Bool from:tonNode.blockIdExt
      to:tonNode.blockIdExt dest_proof:bytes config_proof:bytes
      signatures:liteServer.SignatureSet = liteServer.BlockLink;
    ton.blockId root_cell_hash:int256 file_hash:int256 = ton.BlockId;
    v}

    What is signed is the TL-serialized [ton.blockId], which commits to both
    the block's root hash and the hash of its serialized form. *)

open Ton_cell

type block = { seqno : int32; root_hash : string; file_hash : string }

type signature = { who : string; signature : string }
(** [who] is the signer's ADNL short identifier, not its public key. *)

type outcome = {
  next : block;
  signed_weight : int64;
  total_weight : int64;
  accepted : int;
  offered : int;
}

type error =
  | Boc of Boc.error
  | Merkle of Merkle.error
  | State of State.error
  | Config of Config.error
  | Validators of Validators.error
  | No_proof_of of string
  | No_validator_set
  | Insufficient_weight of { signed : int64; total : int64 }
  | Dest_mismatch of { want : string; got : string }
  | Link_does_not_continue of { trusted : string; starts_at : string }
  | Backward_link

val pp_error : Format.formatter -> error -> unit

val to_sign : block -> string
(** The bytes a validator signs for a block. *)

val validator_set_of_config_proof :
  from_root_hash:string -> config_proof:string -> (Validators.t list, error) result
(** The validator sets a block's state appoints — the current one and, when
    present, the next. Either may legitimately be the set that signed the
    following block, so both are returned and the caller accepts whichever
    reaches the threshold. *)

val verify_forward :
  from_root_hash:string -> config_proof:string -> dest_proof:string -> dest:block ->
  signatures:signature list -> (outcome, error) result
(** Follow one forward link. Succeeds only if signatures carrying more than
    two thirds of a set's total weight verify over [dest]. *)

(** {2 Walking a chain} *)

type link =
  | Forward of {
      source : block;
      dest : block;
      config_proof : string;
      dest_proof : string;
      signatures : signature list;
    }
  | Backward of { source : block; dest : block }
      (** A link from a later block to an earlier one. Walking forward from an
          anchor never needs these, and following one would mean proving an
          older block from a newer one, which this does not implement — so it
          is refused rather than waved through. *)

val follow : block -> link -> (block, error) result
(** Follow one link from a block already trusted. Refuses a link that does not
    start where the trust does. *)

val follow_all : block -> link list -> (block, error) result
(** Follow a chain, threading each link's destination into the next. The
    result is the furthest block the chain establishes. *)
