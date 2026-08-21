(** Immutable bit strings.

    TON cells are addressed in {b bits}, not bytes: a cell holds up to 1023
    bits of data, so every layer above this one needs sub-byte slicing.

    Bit order is big-endian within a byte: bit [0] of a string is the {i most}
    significant bit of byte [0]. This is TON's convention throughout. *)

type t

val empty : t
val length : t -> int
(** Length in {b bits}. *)

val is_empty : t -> bool

(** {2 Access} *)

val get : t -> int -> bool
(** [get b i] is bit [i]. @raise Invalid_argument if out of range. *)

val sub : t -> int -> int -> t
(** [sub b pos len] is the [len] bits starting at [pos].
    @raise Invalid_argument if the range is out of bounds. *)

val concat : t list -> t
val append : t -> t -> t

(** {2 Integers}

    Values are read big-endian, most significant bit first, matching TL-B's
    [uint n] / [int n]. *)

val get_uint : t -> pos:int -> len:int -> int64
(** [len] must be in [0..64]. Reading 0 bits yields [0L]. *)

val get_int : t -> pos:int -> len:int -> int64
(** Two's complement; [len] must be in [0..64]. *)

(** {2 Byte conversion} *)

val of_bytes : string -> t
(** Byte-aligned: [length (of_bytes s) = 8 * String.length s]. *)

val to_bytes : t -> string option
(** [Some] iff the length is a multiple of 8. *)

val to_padded_bytes : t -> string
(** TON's "augmented representation". When the length is not a multiple of 8,
    a single [1] bit is appended followed by [0]s to the byte boundary. A
    byte-aligned value gets {b no} tag — this asymmetry is why the cell
    descriptor's [d2] parity is the only signal that padding is present. *)

val of_padded_bytes : string -> t
(** Strips a completion tag by scanning back for the lowest set bit of the
    last non-zero byte.

    {b Only valid when a tag is known to be present.} Padding is not
    self-describing: a byte-aligned value carries no tag, so ["\x00"] is
    ambiguous between one zero byte and the empty bit string. The Bag-of-Cells
    reader resolves this before calling here — an odd [d2] descriptor means
    padded, an even one means byte-aligned, and the latter must go through
    {!of_bytes} instead. So the round-trip law is conditional:

    {[
      let decode ~padded s = if padded then of_padded_bytes s else of_bytes s in
      decode ~padded:(length b mod 8 <> 0) (to_padded_bytes b) = b
    ]}

    A genuinely padded input always ends in a non-zero byte (the tag bit is
    [1]), so this is total on well-formed input; malformed all-zero input
    decodes to {!empty} rather than raising. *)

(** {2 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
