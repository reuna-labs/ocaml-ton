(** X25519 key agreement from Ed25519 identities.

    Several protocols — ADNL among them — identify peers by an Ed25519
    signing key but derive session secrets with X25519. Doing that requires
    mapping an Edwards public key onto the Montgomery curve, which is what
    {!of_ed25519_pub} does. *)

val of_ed25519_pub : string -> (string, string) result
(** The Curve25519 u-coordinate for an Ed25519 public key.

    Operates on a public value and is therefore not constant time. Fails for
    the one encoding with no Montgomery image, [y = 1]. *)

val scalar_of_ed25519_seed : string -> string
(** The X25519 scalar for an Ed25519 seed: the clamped low half of its
    SHA-512, the same value RFC 8032 derives for signing. *)

val key_exchange : scalar:string -> peer:string -> (string, string) result
(** The shared secret. Fails if the peer's point is of low order, so an
    all-zero secret can never be returned. *)
