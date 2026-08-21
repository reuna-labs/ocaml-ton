(** TON addresses.

    An address is a workchain identifier plus the 32-byte hash of the
    contract's initial state. It has two textual forms:

    - {b raw}, [<workchain>:<64 hex digits>], which carries nothing else;
    - {b user-friendly}, 36 bytes of [tag ‖ workchain ‖ hash ‖ CRC-16] in
      base64, which additionally encodes whether the address is bounceable and
      whether it is testnet-only.

    Those two flags are presentation, not identity: the same contract has
    bounceable and non-bounceable forms that differ in text but are the same
    address. {!t} therefore holds only workchain and hash, and the flags are
    carried separately by {!friendly}. *)

type t = { workchain : int; hash : string }

type error =
  | Bad_raw of string
  | Bad_base64
  | Bad_length of int
  | Bad_tag of int
  | Bad_crc of { expected : string; got : string }

val pp_error : Format.formatter -> error -> unit

val make : workchain:int -> hash:string -> (t, error) result
(** @return [Error] unless [hash] is exactly 32 bytes. *)

(** {2 Raw form} *)

val of_raw : string -> (t, error) result
val to_raw : t -> string

(** {2 User-friendly form} *)

type friendly = { address : t; bounceable : bool; testnet : bool }

val of_friendly : string -> (friendly, error) result
(** Accepts both the standard and URL-safe base64 alphabets, and validates the
    tag byte and CRC-16. *)

val to_friendly : ?bounceable:bool -> ?testnet:bool -> ?url_safe:bool -> t -> string
(** Defaults: [bounceable] and [url_safe] true, [testnet] false — the form
    wallets and explorers normally show. *)

(** {2 Either form} *)

val of_string : string -> (t, error) result
(** Parses whichever form the input is in, discarding the friendly flags. *)

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
(** Prints the raw form, which is the one that identifies an address
    unambiguously. *)

(** {2 Constants} *)

val basechain : int
(** [0] — where ordinary contracts live. *)

val masterchain : int
(** [-1]. *)
