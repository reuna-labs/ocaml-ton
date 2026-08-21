(** Configuration parameters.

    The masterchain state carries the network's configuration as a dictionary
    of parameter cells:

    {v
    masterchain_state_extra#cc26 shard_hashes:ShardHashes config:ConfigParams … = McStateExtra;
    _ config_addr:bits256 config:^(Hashmap 32 ^Cell) = ConfigParams;
    v}

    Parameter 34 is the current validator set and 36 the next; between them
    they say who is entitled to sign a block, which is what a proof chain
    ultimately rests on. *)

open Ton_cell

type error =
  | State of State.error
  | Not_masterchain
  | Not_a_key_block
  | Elided of string
  | Malformed of string

val pp_error : Format.formatter -> error -> unit

val of_state : Cell.t -> (Cell.t, error) result
(** The configuration dictionary root, from a masterchain state. *)

val of_key_block : Cell.t -> (Cell.t, error) result
(** The configuration dictionary root, from a masterchain {e key block}.

    A key block carries the configuration in its own extra rather than only
    in the state it produced:

    {v
    block_extra … custom:(Maybe ^McBlockExtra) = BlockExtra;
    masterchain_block_extra#cca5 key_block:(## 1) shard_hashes:ShardHashes
      shard_fees:ShardFees ^[ … ] config:key_block?ConfigParams = McBlockExtra;
    v}

    This is why block proof chains hop from one key block to the next: the
    block being trusted names, by itself, who may sign the following one, so
    no separate state proof is needed. *)

val param : Cell.t -> int32 -> (Cell.t option, error) result
(** A parameter's cell, given the dictionary root. [None] means the network
    does not set it; a parameter whose branch was pruned is an error rather
    than an absence. *)

val current_validators : int32
val next_validators : int32

val block_extra_tag : int32
(** The implicit constructor tag on [BlockExtra]. A TL-B constructor written
    without an explicit tag still has one — the CRC-32 of its definition, by
    the same rule TL uses. Exposed so the test suite can recompute it from the
    schema instead of trusting a transcribed constant. *)

val block_extra_definition : string
