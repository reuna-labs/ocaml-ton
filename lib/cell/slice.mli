(** Reading cells.

    A slice is a cursor over a cell's bits and references. TL-B is a
    bit-oriented format with heavy use of prefix tags, so parsing is naturally
    written as a sequence of reads with no error plumbing between them; the
    [load_*] functions therefore raise {!Parse_error} and {!parse} converts
    that back into a result at the boundary.

    {[
      Slice.parse cell (fun s ->
          let tag = Slice.load_uint s ~bits:2 in
          let value = Slice.load_uint_z s ~bits:256 in
          (tag, value))
    ]}

    Only use the raising functions inside {!parse}. *)

type t

type error =
  | Not_enough_bits of { want : int; have : int }
  | Not_enough_refs of { have : int }
  | Invalid_width of int
  | Trailing_bits of int
  | Trailing_refs of int
  | Message of string

exception Parse_error of error

val pp_error : Format.formatter -> error -> unit

val parse : Cell.t -> (t -> 'a) -> ('a, error) result
(** Run a parser over a cell, catching {!Parse_error}. *)

val of_cell : Cell.t -> t
val copy : t -> t
(** An independent cursor at the same position, for lookahead. *)

val fail : error -> 'a
(** @raise Parse_error always. *)

(** {2 State} *)

val remaining_bits : t -> int
val remaining_refs : t -> int
val is_empty : t -> bool
val cell : t -> Cell.t
val to_bits : t -> Bits.t
(** The bits not yet consumed. Does not advance the cursor. *)

val refs : t -> Cell.t list
(** The references not yet consumed. Does not advance the cursor. *)

(** {2 Reading}

    All of these raise {!Parse_error} when the slice is exhausted. *)

val load_bit : t -> bool
val preload_bit : t -> bool
val load_bits : t -> int -> Bits.t
val load_bytes : t -> int -> string
val skip : t -> int -> unit

val load_uint : t -> bits:int -> int64
(** [bits] must be in [0..64]. *)

val load_int : t -> bits:int -> int64
val preload_uint : t -> bits:int -> int64

val load_uint_z : t -> bits:int -> Z.t
(** For the wide integers TL-B uses, up to 257 bits. *)

val load_int_z : t -> bits:int -> Z.t

val load_ref : t -> Cell.t
val load_maybe_ref : t -> Cell.t option
(** TL-B's [Maybe ^X]: a presence bit followed by a reference. *)

(** {2 Finishing} *)

val end_parse : t -> unit
(** Assert the slice is fully consumed.
    @raise Parse_error if any bits or references remain. *)
