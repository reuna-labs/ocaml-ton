(** A Unix transport for the liteserver client.

    This is the whole of the IO: a TCP socket, a read loop, and a table of
    outstanding queries. Everything protocol-shaped lives in
    {!Ton_lite_client} and {!Ton_adnl}, which is why this is short.

    Randomness comes from {!Mirage_crypto_rng}; the caller is responsible for
    seeding it (typically [Mirage_crypto_rng_unix.use_default ()]). *)

type t

type error =
  | Client of Ton_lite_client.error
  | Closed of string
  | Timeout

val pp_error : Format.formatter -> error -> unit

val connect :
  ?timeout:float -> host:string -> port:int -> server_pub:string -> unit -> (t, error) result Lwt.t
(** Open a connection and complete the ADNL handshake.

    [server_pub] is the liteserver's 32-byte Ed25519 key — the [key] field of
    a global config entry, base64-decoded. Returns once the server has
    confirmed the handshake with its empty frame; until that arrives there is
    no evidence the keys agree. *)

val call : ?timeout:float -> t -> 'a Ton_lite_client.Query.t -> ('a, error) result Lwt.t
val ping : ?timeout:float -> t -> (int64, error) result Lwt.t
val close : t -> unit Lwt.t

val with_connection :
  ?timeout:float -> host:string -> port:int -> server_pub:string ->
  (t -> ('a, error) result Lwt.t) -> ('a, error) result Lwt.t
