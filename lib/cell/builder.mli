(** Building cells.

    A builder accumulates bits and references and is turned into a {!Cell.t}
    by {!end_cell}. Writes are mutating but return the builder, so they chain:

    {[
      Builder.(create () |> fun b -> store_uint b 0x1234L ~bits:16 |> ...)
    ]}

    Individual writes never fail. If a write would exceed a cell's capacity
    the builder records the first such overflow and ignores subsequent data;
    {!end_cell} then reports it. This keeps building code free of error
    plumbing while still making overflow impossible to ignore. *)

type t

type error =
  | Bit_overflow of { have : int; want : int }
  | Ref_overflow of { have : int }
  | Invalid_width of int
  | Cell of Cell.error

val pp_error : Format.formatter -> error -> unit

val create : unit -> t

(** {2 State} *)

val bit_length : t -> int
val ref_count : t -> int
val available_bits : t -> int
val available_refs : t -> int
val error : t -> error option

(** {2 Writing} *)

val store_bit : t -> bool -> t
val store_bits : t -> Bits.t -> t
val store_bytes : t -> string -> t

val store_uint : t -> int64 -> bits:int -> t
(** Big-endian, most significant bit first. [bits] must be in [0..64]. Bits
    above [bits] in the value are ignored. *)

val store_int : t -> int64 -> bits:int -> t
(** Two's complement. [bits] must be in [0..64]. *)

val store_uint_z : t -> Z.t -> bits:int -> t
(** For values wider than 64 bits, such as the 256-bit hashes and 257-bit
    integers that appear throughout TL-B. Negative values are rejected. *)

val store_int_z : t -> Z.t -> bits:int -> t

val store_ref : t -> Cell.t -> t
val store_maybe_ref : t -> Cell.t option -> t
(** Writes a presence bit, then the reference if present — TL-B's
    [Maybe ^X]. *)

val store_builder : t -> t -> t
(** Appends another builder's bits and references. *)

val store_slice : t -> Slice.t -> t
(** Appends a slice's remaining bits and references. *)

(** {2 Finishing} *)

val to_bits : t -> Bits.t
val end_cell : ?exotic:bool -> t -> (Cell.t, error) result
