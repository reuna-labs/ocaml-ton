(** Currency collections.

    {v
    currencies$_ grams:Grams other:ExtraCurrencyCollection = CurrencyCollection;
    extra_currencies$_ dict:(HashmapE 32 (VarUInteger 32)) = ExtraCurrencyCollection;
    v}

    Every value carried by a message is an amount of TON plus a — nearly
    always empty — dictionary of extra currencies. *)

open Ton_cell

type t = { coins : Z.t; extra : Z.t Dict.t }

val zero : t
val of_coins : Z.t -> t
val load : Slice.t -> t
val store : Builder.t -> t -> (Builder.t, string) result
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
