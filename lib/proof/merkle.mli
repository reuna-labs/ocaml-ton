(** Merkle proofs and updates.

    {v
    !merkle_proof#03  virtual_hash:bits256 depth:uint16 virtual_root:^X = MERKLE_PROOF X;
    !merkle_update#04 old_hash:bits256 new_hash:bits256 old_depth:uint16
                      new_depth:uint16 old:^X new:^X = MERKLE_UPDATE X;
    v}

    The cryptographic work is already done by the time a proof reaches here:
    {!Ton_cell.Cell.make} refuses to build an exotic cell whose stored hashes
    disagree with the cells it references, so a proof that parses at all is
    internally consistent. What remains is to check that it is a proof of the
    thing you asked about — which is the part that is easy to skip and fatal
    to skip. *)

open Ton_cell

type proof = { root : Cell.t; hash : string; depth : int }
(** [hash] is the representation hash of the tree [root] stands for. Pruned
    branches inside [root] report the hashes of the subtrees they replace, so
    [root] hashes exactly as the complete tree would. *)

type update = {
  old_hash : string;
  new_hash : string;
  old_depth : int;
  new_depth : int;
  old_root : Cell.t;
  new_root : Cell.t;
}

type error = Not_a_proof of Cell_type.t | Not_an_update of Cell_type.t | Malformed of string

val pp_error : Format.formatter -> error -> unit
val proof : Cell.t -> (proof, error) result
val update : Cell.t -> (update, error) result
