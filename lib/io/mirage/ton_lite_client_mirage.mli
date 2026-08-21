(** A MirageOS transport for the liteserver client.

    Parameterised over a flow rather than a network stack, so it works with
    anything that implements {!Mirage_flow.S} — a TCP connection, but equally
    a vsock channel out of a confidential guest, or an in-memory pipe replaying
    a recorded session in a test.

    The caller opens the flow; this drives the protocol over it. That split
    exists because everything protocol-shaped already lives in {!Ton_adnl} and
    {!Ton_lite_client}, and it is what keeps the unikernel build free of any
    protocol code of its own. *)

type error =
  | Client of Ton_lite_client.error
  | Closed of string
  | Timeout

val pp_error : Format.formatter -> error -> unit

module Make (F : Mirage_flow.S) : sig
  type t

  val connect :
    ?max_frame:int -> ?max_reads:int -> random:(int -> string) -> F.flow -> server_pub:string ->
    (t, error) result Lwt.t
  (** Complete the ADNL handshake over an already-open flow.

      [random] must be a cryptographic generator: the session's
      confidentiality rests entirely on the ephemeral key and session
      parameters it produces. Under MirageOS that is
      [Mirage_crypto_rng.generate].

      Returns once the server has answered with its empty frame, which is the
      only evidence the keys agree — a wrong key produces a peer that simply
      stays quiet.

      [max_reads] bounds how many reads may pass without the awaited reply
      arriving. It is a bound on reads, not on time: a flow that blocks
      forever will still block, so a deployment that needs a deadline should
      wrap these calls in whatever timer it has. Stated that way because a
      unikernel has no clock unless one is wired in, and pretending otherwise
      would be worse than saying so. *)

  val call : t -> 'a Ton_lite_client.Query.t -> ('a, error) result Lwt.t
  val ping : t -> (int64, error) result Lwt.t
  val close : t -> unit Lwt.t
end
