(** The TL binary wire format.

    TL is the RPC schema language TON uses for ADNL and liteserver traffic. It
    is unrelated to TL-B: TL is byte-oriented and length-prefixed, TL-B is the
    bit-oriented cell layout language.

    Every value is a multiple of four bytes. Integers are little-endian.
    A {e boxed} value carries a four-byte constructor identifier derived from
    the schema text; a {e bare} one does not, and which applies is decided by
    the capitalisation of the type name in the schema. *)

(** {2 Constructor identifiers} *)

module Crc : sig
  val constructor_id : string -> int32
  (** The identifier for a schema definition, computed as CRC-32 of its
      normalised text: comments removed, parentheses removed, any explicit
      [#id] removed, the trailing semicolon removed, and whitespace collapsed.

      Deriving these rather than transcribing them is the whole reason this
      library generates its bindings — a wrong identifier is not a type error,
      it is a liteserver closing the connection without a word. *)

  val normalize : string -> string
  (** The normalised form {!constructor_id} hashes. Exposed because it is the
      part worth inspecting when an identifier disagrees with upstream. *)

  val explicit_id : string -> int32 option
  (** The identifier written into the definition as [name#xxxxxxxx], if there
      is one. A handful of constructors carry one, and it overrides the
      computed value rather than agreeing with it. *)

  val id_of_definition : string -> int32
  (** {!explicit_id} when present, otherwise {!constructor_id}. This is the
      identifier that actually goes on the wire. *)
end

(** {2 Reading} *)

module Reader : sig
  type t

  type error =
    | Truncated of { field : string; want : int; have : int }
    | Bad_constructor of { expected : int32; got : int32 }
    | Bad_length_prefix of int
    | Bad_padding
    | Bad_bool of int32
    | Message of string

  exception Error of error

  val pp_error : Format.formatter -> error -> unit

  val make : string -> t
  val parse : string -> (t -> 'a) -> ('a, error) result
  (** Run a decoder, converting {!Error} back into a result. All the [read_*]
      functions raise, so that generated decoders read as straight-line code. *)

  val fail : error -> 'a
  val remaining : t -> int
  val at_end : t -> bool
  val finish : t -> unit
  (** Assert the input is fully consumed. *)

  val int : t -> int32
  val nat : t -> int32
  (** A [#] field. Same encoding as {!int}; distinguished because it carries
      conditional-field flags rather than a number. *)

  val long : t -> int64
  val double : t -> float
  val int128 : t -> string
  val int256 : t -> string
  val bytes : t -> string
  val string : t -> string
  val bool : t -> bool
  val constructor : t -> int32
  val expect : t -> int32 -> unit
  val vector : t -> (t -> 'a) -> 'a list
end

(** {2 Writing} *)

module Writer : sig
  type t

  val make : unit -> t
  val contents : t -> string
  val to_string : (t -> unit) -> string
  val int : t -> int32 -> unit
  val nat : t -> int32 -> unit
  val long : t -> int64 -> unit
  val double : t -> float -> unit
  val int128 : t -> string -> unit
  val int256 : t -> string -> unit
  val bytes : t -> string -> unit
  val string : t -> string -> unit
  val bool : t -> bool -> unit
  val constructor : t -> int32 -> unit
  val vector : t -> (t -> 'a -> unit) -> 'a list -> unit
end
