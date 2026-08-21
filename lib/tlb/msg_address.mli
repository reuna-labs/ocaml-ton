(** Addresses as they appear inside cells.

    {v
    addr_none$00 = MsgAddressExt;
    addr_extern$01 len:(## 9) external_address:(bits len) = MsgAddressExt;
    addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 address:bits256 = MsgAddressInt;
    addr_var$11 anycast:(Maybe Anycast) addr_len:(## 9) workchain_id:int32
                address:(bits addr_len) = MsgAddressInt;
    anycast_info$_ depth:(#<= 30) { depth >= 1 } rewrite_pfx:(bits depth) = Anycast;
    v}

    Almost everything in practice is [addr_std] without anycast; the other
    three still have to be parsed, because they appear in real messages. *)

open Ton_cell

type anycast = { depth : int; rewrite_pfx : Bits.t }

type t =
  | Addr_none
  | Addr_extern of Bits.t
  | Addr_std of { anycast : anycast option; workchain : int; address : string }
  | Addr_var of { anycast : anycast option; workchain : int; address : Bits.t }

val of_address : Ton_address.t -> t
(** The [addr_std] form, without anycast. *)

val to_address : t -> Ton_address.t option
(** [None] for the external forms, and for [addr_var] whose address is not
    exactly 256 bits. *)

val load : Slice.t -> t
(** @raise Slice.Parse_error on malformed input. *)

val store : Builder.t -> t -> Builder.t
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
