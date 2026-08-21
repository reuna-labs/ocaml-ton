module Lite = Ton_tl_schema.Lite
module Adnl_tl = Ton_tl_schema.Adnl
module R = Ton_tl.Tl.Reader
module W = Ton_tl.Tl.Writer

type error =
  | Tl of R.error
  | Server of { code : int32; message : string }
  | Adnl of Ton_adnl.Conn.error
  | Unexpected of string

let pp_error ppf = function
  | Tl e -> R.pp_error ppf e
  | Server { code; message } -> Format.fprintf ppf "liteserver error %ld: %s" code message
  | Adnl e -> Ton_adnl.Conn.pp_error ppf e
  | Unexpected m -> Format.pp_print_string ppf m

let method_id name = Int64.logor (Int64.of_int (Web3_codec.Crc.crc16_xmodem name)) 0x10000L

module Query = struct
  type 'a t = { encode : string; decode : string -> ('a, error) result }

  let encode q = q.encode
  let decode q s = q.decode s

  let account_id (a : Ton_address.t) : Lite.lite_server_account_id =
    { workchain = Int32.of_int a.workchain; id = a.hash }

  (* Every answer is a union of the expected result and liteServer.error, so
     the identifier has to be inspected before committing to a decoder. *)
  let make ~encode ~read =
    { encode;
      decode =
        (fun s ->
          match
            R.parse s (fun r ->
                let id = R.constructor r in
                if id = Lite.lite_server_error_id then
                  let e = Lite.read_lite_server_error r in
                  Error (Server { code = e.Lite.code; message = e.Lite.message })
                else Ok (read id r))
          with
          | Error e -> Error (Tl e)
          | Ok v -> v) }

  let simple ~encode ~expect ~read =
    make ~encode ~read:(fun id r ->
        if id <> expect then
          raise (R.Error (R.Message (Printf.sprintf "unexpected constructor %08lx" id)));
        read r)

  let boxed f v = W.to_string (fun w -> f w v)

  let get_masterchain_info =
    simple
      ~encode:(boxed Lite.write_boxed_lite_server_get_masterchain_info ())
      ~expect:Lite.lite_server_masterchain_info_id ~read:Lite.read_lite_server_masterchain_info

  let get_time =
    simple
      ~encode:(boxed Lite.write_boxed_lite_server_get_time ())
      ~expect:Lite.lite_server_current_time_id ~read:Lite.read_lite_server_current_time

  let get_version =
    simple
      ~encode:(boxed Lite.write_boxed_lite_server_get_version ())
      ~expect:Lite.lite_server_version_id ~read:Lite.read_lite_server_version

  let get_block id =
    simple
      ~encode:(boxed Lite.write_boxed_lite_server_get_block { Lite.id })
      ~expect:Lite.lite_server_block_data_id ~read:Lite.read_lite_server_block_data

  let get_block_header id ~mode =
    simple
      ~encode:(boxed Lite.write_boxed_lite_server_get_block_header { Lite.id; mode })
      ~expect:Lite.lite_server_block_header_id ~read:Lite.read_lite_server_block_header

  let get_account_state ~block account =
    simple
      ~encode:
        (boxed Lite.write_boxed_lite_server_get_account_state
           { Lite.id = block; account = account_id account })
      ~expect:Lite.lite_server_account_state_id ~read:Lite.read_lite_server_account_state

  let run_smc_method ~block account ~method_id ~params ~mode =
    simple
      ~encode:
        (boxed Lite.write_boxed_lite_server_run_smc_method
           { Lite.mode; id = block; account = account_id account; method_id; params })
      ~expect:Lite.lite_server_run_method_result_id ~read:Lite.read_lite_server_run_method_result

  let send_message body =
    simple
      ~encode:(boxed Lite.write_boxed_lite_server_send_message { Lite.body })
      ~expect:Lite.lite_server_send_msg_status_id ~read:Lite.read_lite_server_send_msg_status

  let get_transactions ~count ~account ~lt ~hash =
    simple
      ~encode:
        (boxed Lite.write_boxed_lite_server_get_transactions
           { Lite.count; account = account_id account; lt; hash })
      ~expect:Lite.lite_server_transaction_list_id ~read:Lite.read_lite_server_transaction_list

  let lookup_block id ~mode ?lt ?utime () =
    simple
      ~encode:(boxed Lite.write_boxed_lite_server_lookup_block { Lite.mode; id; lt; utime })
      ~expect:Lite.lite_server_block_header_id ~read:Lite.read_lite_server_block_header

  let get_config_params ~block ~mode param_list =
    simple
      ~encode:
        (boxed Lite.write_boxed_lite_server_get_config_params { Lite.mode; id = block; param_list })
      ~expect:Lite.lite_server_config_info_id ~read:Lite.read_lite_server_config_info

  let get_all_shards_info id =
    simple
      ~encode:(boxed Lite.write_boxed_lite_server_get_all_shards_info { Lite.id })
      ~expect:Lite.lite_server_all_shards_info_id ~read:Lite.read_lite_server_all_shards_info

  let get_one_transaction ~block ~account ~lt =
    simple
      ~encode:
        (boxed Lite.write_boxed_lite_server_get_one_transaction
           { Lite.id = block; account = account_id account; lt })
      ~expect:Lite.lite_server_transaction_info_id ~read:Lite.read_lite_server_transaction_info
end

module Session = struct
  type t = { conn : Ton_adnl.Conn.t }

  type event =
    | Answer of { query_id : string; body : string }
    | Pong of int64
    | Empty

  let create conn = { conn }
  let conn t = t.conn

  (* method -> liteServer.query -> adnl.message.query -> frame *)
  let query t ~query_id ~nonce q =
    let inner = W.to_string (fun w -> Lite.write_boxed_lite_server_query w { Lite.data = Query.encode q }) in
    let outer =
      W.to_string (fun w -> Lite.write_boxed_adnl_message_query w { Lite.query_id; query = inner })
    in
    let conn, bytes = Ton_adnl.Conn.send t.conn ~nonce outer in
    ({ conn }, bytes)

  let ping t ~random_id ~nonce =
    let msg = W.to_string (fun w -> Adnl_tl.write_boxed_tcp_ping w { Adnl_tl.random_id }) in
    let conn, bytes = Ton_adnl.Conn.send t.conn ~nonce msg in
    ({ conn }, bytes)

  let decode_frame payload =
    if payload = "" then Ok Empty
    else
      match
        R.parse payload (fun r ->
            let id = R.constructor r in
            if id = Lite.adnl_message_answer_id then
              let a = Lite.read_adnl_message_answer r in
              Answer { query_id = a.Lite.query_id; body = a.Lite.answer }
            else if id = Adnl_tl.tcp_pong_id then Pong (Adnl_tl.read_tcp_pong r).Adnl_tl.random_id
            else if id = Adnl_tl.tcp_ping_id then Pong (Adnl_tl.read_tcp_ping r).Adnl_tl.random_id
            else raise (R.Error (R.Message (Printf.sprintf "unexpected ADNL message %08lx" id))))
      with
      | Ok e -> Ok e
      | Error e -> Error (Tl e)

  let feed t data =
    match Ton_adnl.Conn.recv t.conn data with
    | Error e -> Error (Adnl e)
    | Ok (conn, payloads) ->
        let rec go acc = function
          | [] -> Ok ({ conn }, List.rev acc)
          | p :: rest -> ( match decode_frame p with Error e -> Error e | Ok ev -> go (ev :: acc) rest)
        in
        go [] payloads
end
