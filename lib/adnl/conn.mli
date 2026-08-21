(** An ADNL connection as a state machine.

    Bytes in, bytes out. Nothing here opens a socket, reads a clock or draws
    randomness: the ephemeral key, the session parameters and every frame
    nonce are arguments. A session is therefore reproducible from its inputs,
    which is what lets an encrypted conversation be replayed from a recorded
    transcript with no network at all.

    The cost is that a transport has to write the read/write loop itself,
    which is about sixty lines. The benefit is that the protocol can be
    tested without one. *)

type t

type error = Handshake of Handshake.error | Frame of Frame.error

val pp_error : Format.formatter -> error -> unit

val connect :
  ?max_frame:int -> server_pub:string -> ephemeral_seed:string -> aes_params:string -> unit ->
  (t * string, error) result
(** [connect ~server_pub ~ephemeral_seed ~aes_params] is the connection and
    the 256 bytes to send before anything else.

    [ephemeral_seed] is 32 bytes and [aes_params] 160, both of which must come
    from a cryptographic generator — the session's confidentiality rests
    entirely on them. [max_frame] bounds how much a peer can make us allocate
    and defaults to {!Frame.default_max_size}. *)

val send : t -> nonce:string -> string -> t * string
(** Wrap a payload into a frame and encrypt it.
    @raise Invalid_argument if [nonce] is not 32 bytes. *)

val recv : t -> string -> (t * string list, error) result
(** Feed bytes from the peer and take whatever complete frames they finish.
    Partial frames are retained, so the caller may pass whatever a socket
    returned without regard for frame boundaries. *)

val confirmed : t -> bool
(** Whether the peer has sent a frame yet.

    A server answers a good handshake with an empty frame. Receiving it is
    the only proof that it derived the same session parameters, and hence
    that the handshake succeeded — there is no acknowledgement otherwise, and
    a wrong key simply produces a connection that goes quiet. *)

val buffered : t -> int
(** Bytes held back as an incomplete frame. *)
