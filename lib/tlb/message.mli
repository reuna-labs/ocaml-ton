(** State init and messages.

    {v
    tick_tock$_ tick:Bool tock:Bool = TickTock;
    _ split_depth:(Maybe (## 5)) special:(Maybe TickTock)
      code:(Maybe ^Cell) data:(Maybe ^Cell) library:(Maybe ^Cell) = StateInit;

    int_msg_info$0 ihr_disabled:Bool bounce:Bool bounced:Bool
      src:MsgAddressInt dest:MsgAddressInt value:CurrencyCollection
      ihr_fee:Grams fwd_fee:Grams created_lt:uint64 created_at:uint32 = CommonMsgInfo;
    ext_in_msg_info$10 src:MsgAddressExt dest:MsgAddressInt import_fee:Grams = CommonMsgInfo;
    ext_out_msg_info$11 src:MsgAddressInt dest:MsgAddressExt
      created_lt:uint64 created_at:uint32 = CommonMsgInfo;

    message$_ {X:Type} info:CommonMsgInfo init:(Maybe (Either StateInit ^StateInit))
      body:(Either X ^X) = Message X;
    v}

    On master, [split_depth] has been renamed [fixed_prefix_length] and
    [ihr_fee] renamed [extra_flags]; both are wire-compatible, and the older
    names are kept here because they are what the documentation and other
    SDKs use. *)

open Ton_cell

type tick_tock = { tick : bool; tock : bool }

type state_init = {
  split_depth : int option;
  special : tick_tock option;
  code : Cell.t option;
  data : Cell.t option;
  library : Cell.t option;
}

val empty_state_init : state_init
val load_state_init : Slice.t -> state_init
val store_state_init : Builder.t -> state_init -> Builder.t

val state_init_address : workchain:int -> state_init -> (Ton_address.t, string) result
(** A contract's address is the hash of its initial state, so deploying is
    just sending to the address its own code and data determine. *)

type info =
  | Internal of {
      ihr_disabled : bool;
      bounce : bool;
      bounced : bool;
      src : Msg_address.t;
      dest : Msg_address.t;
      value : Currency.t;
      ihr_fee : Z.t;
      fwd_fee : Z.t;
      created_lt : int64;
      created_at : int32;
    }
  | External_in of { src : Msg_address.t; dest : Msg_address.t; import_fee : Z.t }
  | External_out of {
      src : Msg_address.t;
      dest : Msg_address.t;
      created_lt : int64;
      created_at : int32;
    }

val load_info : Slice.t -> info
val store_info : Builder.t -> info -> (Builder.t, string) result

type t = { info : info; init : state_init option; body : Cell.t }
(** [body] is always held as a cell. Whether it is stored inline or behind a
    reference is an encoding decision made when serializing, not part of the
    message's meaning. *)

val load : Slice.t -> t
val store : Builder.t -> t -> (Builder.t, string) result
(** Stores the body inline when it fits alongside everything else, and behind
    a reference otherwise — the choice every SDK makes. *)

val to_cell : t -> (Cell.t, string) result
val of_cell : Cell.t -> (t, Slice.error) result
