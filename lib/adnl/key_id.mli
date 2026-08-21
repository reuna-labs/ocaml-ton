(** ADNL short identifiers.

    A peer is named by the SHA-256 of its boxed TL public key, not by the key
    itself. The handshake opens with the server's, which is how the server
    knows which of its keys the packet is addressed to. *)

val of_ed25519_pub : string -> string
(** The 32-byte identifier for an Ed25519 public key.
    @raise Invalid_argument if the key is not 32 bytes. *)
