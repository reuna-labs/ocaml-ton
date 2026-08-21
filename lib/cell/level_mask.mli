(** Cell level masks.

    A cell exists at up to four levels (0..3); the mask records which of them
    are {i significant}, meaning the cell has a distinct hash there. Ordinary
    cells inherit the union of their children's masks; pruned branches carry
    theirs explicitly. *)

type t = private int

val v : int -> t
val value : t -> int
val level : t -> int
(** Number of significant bits in the mask: [32 - clz32 mask]. *)

val hash_index : t -> int
(** Population count of the mask. *)

val hash_count : t -> int
(** [hash_index + 1] — how many distinct hashes the cell stores. *)

val apply : t -> int -> t
(** [apply m l] keeps only the mask bits below level [l]. *)

val is_significant : t -> int -> bool
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
