(** Cell types.

    An {b ordinary} cell holds plain data. The four {b exotic} types carry a
    type tag in their first 8 bits and change how hashing works: pruned
    branches stand in for elided subtrees in a Merkle proof, and Merkle
    proof/update cells hash their children one level up. *)

type t = Ordinary | Pruned_branch | Library | Merkle_proof | Merkle_update

val to_tag : t -> int
(** The value of the leading type byte. Not meaningful for [Ordinary]. *)

val of_tag : int -> t option
val is_exotic : t -> bool
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
