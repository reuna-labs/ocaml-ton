(** Bag of Cells — the container format TON uses to serialize a cell DAG.

    Cells are stored in topological order with references encoded as indices
    into that order, so a reference always points {i forward} to a
    higher-numbered cell. Reading is deliberately strict: a Bag of Cells
    arriving from a liteserver is untrusted input, so every index, length and
    checksum is validated and no malformed input raises. *)

type error =
  | Bad_magic of int
  | Truncated of { field : string; want : int; have : int }
  | Unsupported_size of { field : string; got : int }
  | Bad_flags of int
  | Cell_too_large of { index : int; error : Cell.error }
  | Backward_ref of { at : int; target : int }
  | Ref_out_of_range of { at : int; target : int; cells : int }
  | Root_out_of_range of { root : int; cells : int }
  | No_roots
  | Multiple_roots of int
  | Bad_crc of { expected : string; got : string }
  | Trailing_bytes of int

val pp_error : Format.formatter -> error -> unit

val deserialize : string -> (Cell.t list, error) result
(** Parse a Bag of Cells, returning its roots in declared order. *)

val deserialize_root : string -> (Cell.t, error) result
(** As {!deserialize}, but requires exactly one root. *)

val serialize : ?idx:bool -> ?crc32:bool -> Cell.t -> string
(** Serialize a single-root Bag of Cells.

    [idx] (default [false]) writes the optional cell-offset index, and
    [crc32] (default [false]) appends a CRC-32C checksum. Neither affects
    which cells are written or their order, so all four combinations
    round-trip to the same DAG. *)

val topological_sort : Cell.t -> (Cell.t * int list) list
(** The cells reachable from a root, deduplicated by hash and ordered so that
    every reference points to a later entry. Each entry pairs a cell with the
    indices of its references. Exposed because the ordering is observable in
    the serialized bytes. *)
