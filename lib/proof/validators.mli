(** Validator sets.

    {v
    validator#53      public_key:SigPubKey weight:uint64 = ValidatorDescr;
    validator_addr#73 public_key:SigPubKey weight:uint64 adnl_addr:bits256 = ValidatorDescr;
    validators#11     utime_since:uint32 utime_until:uint32 total:(## 16)
                      main:(## 16) list:(Hashmap 16 ValidatorDescr) = ValidatorSet;
    validators_ext#12 utime_since:uint32 utime_until:uint32 total:(## 16)
                      main:(## 16) total_weight:uint64
                      list:(HashmapE 16 ValidatorDescr) = ValidatorSet;
    ed25519_pubkey#8e81278a pubkey:bits256 = SigPubKey;
    v}

    A validator is identified in a signature by the short identifier of its
    key, not by the key itself, so the set has to be indexed that way. *)

open Ton_cell

type validator = { public_key : string; weight : int64; short_id : string }

type t = {
  utime_since : int32;
  utime_until : int32;
  total : int;
  main : int;
  total_weight : int64;
  list : validator list;
}

type error = Elided of string | Malformed of string

val pp_error : Format.formatter -> error -> unit
val of_cell : Cell.t -> (t, error) result

val main_list : t -> validator list
(** The validators entitled to sign, which is the first [main] of the set
    rather than all [total] of them. TON appoints a larger set and draws the
    signing subset from its front, so using the whole set as the denominator
    puts the two-thirds threshold permanently out of reach. *)

val main_weight : t -> int64
(** Total weight of {!main_list} — the denominator the threshold is against. *)

val find : t -> short_id:string -> validator option
(** Searches {!main_list}: a signature from outside the signing subset does
    not count toward the threshold. *)
