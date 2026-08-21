(** Currency amounts.

    {v
    var_uint$_ {n:#} len:(#< n) value:(uint (len*8)) = VarUInteger n;
    nanograms$_ amount:(VarUInteger 16) = Grams;
    _ grams:Grams = Coins;
    v}

    A [VarUInteger n] is a byte count followed by that many bytes, so small
    amounts cost little space. The length field is [ceil(log2 n)] bits wide —
    four bits for the [VarUInteger 16] that carries TON amounts. Zero encodes
    as a length of zero with no value bits at all, and the encoder must use
    the fewest bytes that fit or the encoding is not canonical. *)

open Ton_cell

type error = Negative of Z.t | Too_large of { value : Z.t; max_bytes : int }

val pp_error : Format.formatter -> error -> unit

val load_var_uint : Slice.t -> n:int -> Z.t
(** @raise Slice.Parse_error on a truncated slice. *)

val store_var_uint : Builder.t -> n:int -> Z.t -> (Builder.t, error) result

val load_coins : Slice.t -> Z.t
(** [VarUInteger 16], the amount in nanotons. *)

val store_coins : Builder.t -> Z.t -> (Builder.t, error) result

(** {2 Human-readable amounts}

    One TON is 10^9 nanotons. *)

val nano_per_ton : Z.t

val to_string : Z.t -> string
(** Decimal TON with trailing zeros trimmed, e.g. [1500000000] renders as
    ["1.5"]. *)

val of_string : string -> (Z.t, string) result
(** Parses decimal TON into nanotons; rejects more than nine decimal places
    rather than rounding. *)
