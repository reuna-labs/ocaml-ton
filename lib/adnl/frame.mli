(** ADNL framing.

    A frame is

    {v
    size     : uint32 little-endian, counting everything after itself
    nonce    : 32 random bytes
    payload  : size - 64 bytes
    checksum : SHA-256 of nonce ‖ payload
    v}

    The length prefix is inside the encrypted stream, so a peer cannot even
    see where frames begin without the session keys. The nonce exists so that
    the checksum of a repeated payload differs. *)

type error =
  | Too_large of { size : int; limit : int }
  | Too_small of int
  | Bad_checksum

val pp_error : Format.formatter -> error -> unit

val overhead : int
(** [64] — the nonce and checksum a payload is wrapped in. *)

val default_max_size : int
(** [262144]. The protocol allows up to 2^24, but a unikernel should not let
    a peer choose how much it allocates. *)

val encode : nonce:string -> string -> string
(** The plaintext frame. The caller encrypts it.
    @raise Invalid_argument if [nonce] is not 32 bytes. *)

val decode : max_size:int -> string -> (( string * int ) option, error) result
(** [decode ~max_size buf] reads one frame from the front of a decrypted
    buffer, returning the payload and how many bytes it consumed, or [None]
    if the buffer does not yet hold a whole frame. *)
