(** TL-B hashmaps.

    A TON dictionary is a binary Patricia trie over fixed-width keys, stored
    as a cell tree. Each edge carries a label — a shared key prefix — encoded
    in whichever of three forms is shortest:

    {v
    hml_short$0  {m:#} {n:#} len:(Unary ~n) s:(n * Bit)
    hml_long$10  {m:#} n:(#<= m) s:(n * Bit)
    hml_same$11  {m:#} v:Bit n:(#<= m)
    v}

    Choosing the shortest label is required, not an optimisation: it is part
    of the canonical encoding, so two implementations that disagree produce
    different cell hashes for the same map.

    Keys are non-negative integers of exactly {!key_bits} bits. *)

open Ton_cell

type 'v t

val empty : key_bits:int -> 'v t
val key_bits : 'v t -> int
val cardinal : 'v t -> int
val is_empty : 'v t -> bool

val is_partial : 'v t -> bool
(** Whether any branch was elided when this dictionary was parsed.

    Dictionaries inside a Merkle proof have subtrees replaced by pruned
    branches. Those are skipped while parsing, which means a key can be
    missing because it is genuinely absent {i or} because it was pruned away.
    The reference implementation does not distinguish the two; we track it,
    because for a light client "not in the map" and "not in the part of the
    map I was given" are very different claims. *)

(** {2 Construction and lookup} *)

val of_list : key_bits:int -> (Z.t * 'v) list -> 'v t
val to_list : 'v t -> (Z.t * 'v) list
(** Ascending by key. *)

val find : Z.t -> 'v t -> 'v option
val mem : Z.t -> 'v t -> bool
val add : Z.t -> 'v -> 'v t -> 'v t
val remove : Z.t -> 'v t -> 'v t
val fold : (Z.t -> 'v -> 'a -> 'a) -> 'v t -> 'a -> 'a
val map : ('v -> 'w) -> 'v t -> 'w t

(** {2 Reading}

    These raise {!Slice.Parse_error}; call them inside {!Slice.parse}. *)

val load : Slice.t -> key_bits:int -> value:(Slice.t -> 'v) -> 'v t
(** [Hashmap n X] — the edge is read from the slice's current position. *)

val load_maybe : Slice.t -> key_bits:int -> value:(Slice.t -> 'v) -> 'v t
(** [HashmapE n X] — a presence bit, then the root in a reference. *)

val of_cell : Cell.t -> key_bits:int -> value:(Slice.t -> 'v) -> ('v t, Slice.error) result
(** Parse a whole cell as a [Hashmap n X] root. *)

(** {2 Writing} *)

type error = Empty_hashmap | Key_out_of_range of Z.t | Builder of Builder.error

val pp_error : Format.formatter -> error -> unit

val store : Builder.t -> value:(Builder.t -> 'v -> Builder.t) -> 'v t -> (Builder.t, error) result
(** [Hashmap n X]. Fails on an empty dictionary, which the type cannot
    represent — use {!store_maybe}. *)

val store_maybe : Builder.t -> value:(Builder.t -> 'v -> Builder.t) -> 'v t -> (Builder.t, error) result
(** [HashmapE n X]. *)

val to_cell : value:(Builder.t -> 'v -> Builder.t) -> 'v t -> (Cell.t, error) result
