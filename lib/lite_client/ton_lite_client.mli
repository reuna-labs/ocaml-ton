(** The liteserver protocol, with no IO.

    A liteserver query travels inside three envelopes: the method itself, a
    [liteServer.query] wrapper, and an [adnl.message.query] carrying a
    correlation identifier. Answers come back as [adnl.message.answer] whose
    body is either the boxed result or a boxed [liteServer.error] — so every
    response has to be decoded as that union, not as the result alone.

    Like {!Ton_adnl.Conn}, nothing here touches a socket, a clock or a random
    generator. Query identifiers and frame nonces are arguments. *)

module Lite = Ton_tl_schema.Lite

type error =
  | Tl of Ton_tl.Tl.Reader.error
  | Server of { code : int32; message : string }
  | Adnl of Ton_adnl.Conn.error
  | Unexpected of string

val pp_error : Format.formatter -> error -> unit

(** {2 Queries} *)

module Query : sig
  type 'a t

  val encode : 'a t -> string
  (** The boxed method, ready to be wrapped for sending. *)

  val decode : 'a t -> string -> ('a, error) result
  (** Decode an answer, turning a [liteServer.error] into {!Server}. *)

  val account_id : Ton_address.t -> Lite.lite_server_account_id

  val get_masterchain_info : Lite.lite_server_masterchain_info t
  val get_time : Lite.lite_server_current_time t
  val get_version : Lite.lite_server_version t
  val get_block : Lite.ton_node_block_id_ext -> Lite.lite_server_block_data t
  val get_block_header : Lite.ton_node_block_id_ext -> mode:int32 -> Lite.lite_server_block_header t

  val get_account_state :
    block:Lite.ton_node_block_id_ext -> Ton_address.t -> Lite.lite_server_account_state t

  val run_smc_method :
    block:Lite.ton_node_block_id_ext -> Ton_address.t -> method_id:int64 -> params:string ->
    mode:int32 -> Lite.lite_server_run_method_result t
  (** [params] and the result are Bags of Cells holding a TVM stack. The
      method runs on the server, which is why no local TVM is needed; the
      corollary is that its result cannot be verified without one. *)

  val send_message : string -> Lite.lite_server_send_msg_status t
  (** The argument is a serialized external message. *)

  val get_transactions :
    count:int32 -> account:Ton_address.t -> lt:int64 -> hash:string -> Lite.lite_server_transaction_list t

  val lookup_block :
    Lite.ton_node_block_id -> mode:int32 -> ?lt:int64 -> ?utime:int32 -> unit ->
    Lite.lite_server_block_header t

  val get_config_params :
    block:Lite.ton_node_block_id_ext -> mode:int32 -> int32 list -> Lite.lite_server_config_info t

  val get_all_shards_info : Lite.ton_node_block_id_ext -> Lite.lite_server_all_shards_info t
  val get_one_transaction :
    block:Lite.ton_node_block_id_ext -> account:Ton_address.t -> lt:int64 ->
    Lite.lite_server_transaction_info t
end

val method_id : string -> int64
(** The identifier a get-method is called by: CRC-16/XMODEM of its name with
    bit 16 set. *)

(** {2 Sessions} *)

module Session : sig
  type t

  type event =
    | Answer of { query_id : string; body : string }
        (** [body] still needs {!Query.decode}; the session does not know
            which query an identifier belongs to. *)
    | Pong of int64
    | Empty
        (** A frame with no payload. The first one is the server confirming
            the handshake. *)

  val create : Ton_adnl.Conn.t -> t
  val conn : t -> Ton_adnl.Conn.t

  val query : t -> query_id:string -> nonce:string -> 'a Query.t -> t * string
  (** The bytes to send. [query_id] is 32 bytes and correlates the answer. *)

  val ping : t -> random_id:int64 -> nonce:string -> t * string
  val feed : t -> string -> (t * event list, error) result
end
