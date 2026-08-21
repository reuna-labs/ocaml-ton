(** TON cells and the representation hash.

    A cell holds up to {!max_bits} bits of data and up to {!max_refs}
    references to other cells, forming a DAG. Its {e representation hash} is
    what addresses, signatures and Merkle proofs are all ultimately built on.

    Cells are immutable and can only be built through {!make}, which validates
    the limits and exotic layouts and computes all hashes eagerly. That makes
    {!hash} and {!depth} O(1) and means a value of this type cannot exist with
    a hash that disagrees with its contents. *)

type t

val max_bits : int
(** [1023]. *)

val max_refs : int
(** [4]. *)

val max_depth : int
(** [1024]. *)

type error =
  | Too_many_bits of int
  | Too_many_refs of int
  | Depth_overflow of int
  | Exotic_too_short of int
  | Exotic_unknown_type of int
  | Pruned_has_refs of int
  | Pruned_bad_level of int
  | Pruned_bad_size of { expected : int; got : int }
  | Merkle_proof_bad_size of int
  | Merkle_proof_bad_refs of int
  | Merkle_update_bad_size of int
  | Merkle_update_bad_refs of int
  | Merkle_hash_mismatch of int
  | Merkle_depth_mismatch of int
  | Library_bad_size of int
  | Invalid_hash_layout

val pp_error : Format.formatter -> error -> unit

val make : exotic:bool -> Bits.t -> t list -> (t, error) result
(** [make ~exotic bits refs] builds a cell. When [exotic] is set the leading 8
    bits are read as the type tag and the corresponding layout is validated —
    including, for Merkle proofs and updates, that the stored hashes and
    depths actually match the referenced cells. *)

val empty : t
(** The cell with no data and no references. *)

(** {2 Access} *)

val bits : t -> Bits.t
val refs : t -> t list
val ref_count : t -> int
val nth_ref : t -> int -> t option
val cell_type : t -> Cell_type.t
val is_exotic : t -> bool
val mask : t -> Level_mask.t
val level : t -> int

val hash : ?level:int -> t -> string
(** The 32-byte hash at [level], defaulting to [0] — the {e representation
    hash}, which is what TON addresses, signatures and Merkle proofs are built
    on.

    {b This is not a cell identity.} A pruned branch reports the hash of the
    subtree it replaces at level 0, so a pruned branch and the real subtree
    share a level-0 hash while being different cells. Use {!identity} to tell
    cells apart; see the note there.

    Beware when cross-checking against ton-core: its [hash()] defaults to
    level {b 3}, not 0.

    @raise Invalid_argument if [level] is outside [0..3]. *)

val depth : ?level:int -> t -> int
(** Defaults to level [0], matching {!hash}.
    @raise Invalid_argument if [level] is outside [0..3]. *)

val identity : t -> string
(** The cell's own top-level hash — [hash ~level:3].

    Unlike {!hash}, this distinguishes a pruned branch from the subtree it
    stands in for, because it commits to the cell's actual stored data. It is
    therefore the correct key for deduplication, hash tables and equality. For
    an ordinary cell of level 0 — the common case — it coincides with the
    representation hash. *)

(** {2 Comparison} *)

val equal : t -> t -> bool
(** Equality of {!identity}, so a pruned branch is never equal to the subtree
    it replaces. *)

val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
