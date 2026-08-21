(** The ADNL handshake.

    The client sends 256 plaintext bytes:

    {v
    [0,32)    the server's ADNL short identifier
    [32,64)   the client's ephemeral Ed25519 public key
    [64,96)   SHA-256 of the session parameters
    [96,256)  the session parameters, encrypted
    v}

    The shared secret is X25519 between the client's ephemeral key and the
    server's advertised Ed25519 key, both mapped onto the Montgomery curve.
    Because the server can derive the same secret, it can decrypt the session
    parameters; proving it did is the point of the empty frame it replies
    with.

    Both sources of randomness are arguments rather than drawn from a
    generator, which is what makes a session reproducible. *)

type error =
  | Bad_server_key of string
  | Bad_seed of string
  | Bad_aes_params of int
  | Key_exchange of string

val pp_error : Format.formatter -> error -> unit

val packet_size : int
(** [256]. *)

val params_size : int
(** [160] — two keys, two initial counters, and padding. *)

val build :
  server_pub:string -> ephemeral_seed:string -> aes_params:string ->
  (string * Ctr.t * Ctr.t, error) result
(** Returns the packet to send and the receive and transmit keystreams. *)
