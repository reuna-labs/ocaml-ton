open Lwt.Infix
module C = Ton_lite_client

type error = Client of C.error | Closed of string | Timeout

let pp_error ppf = function
  | Client e -> C.pp_error ppf e
  | Closed m -> Format.fprintf ppf "connection closed: %s" m
  | Timeout -> Format.pp_print_string ppf "timed out"

module Make (F : Mirage_flow.S) = struct
  type t = {
    flow : F.flow;
    mutable session : C.Session.t;
    mutable closed : string option;
    random : int -> string;
    max_reads : int;
  }

  let write t s =
    F.write t.flow (Cstruct.of_string s) >|= function
    | Ok () -> Ok ()
    | Error e -> Error (Closed (Format.asprintf "%a" F.pp_write_error e))

  let read t =
    F.read t.flow >|= function
    | Ok (`Data cs) -> Ok (Cstruct.to_string cs)
    | Ok `Eof -> Error (Closed "peer closed the connection")
    | Error e -> Error (Closed (Format.asprintf "%a" F.pp_error e))

  (* One read at a time, feeding whatever arrives into the session. Frames do
     not align with reads, so this returns however many completed. *)
  let pump t =
    read t >|= function
    | Error _ as e -> e
    | Ok data -> (
        match C.Session.feed t.session data with
        | Error e -> Error (Client e)
        | Ok (session, events) ->
            t.session <- session;
            Ok events)

  let rec await t ~budget ~pick =
    if budget <= 0 then Lwt.return (Error Timeout)
    else
      pump t >>= function
      | Error e ->
          t.closed <- Some (Format.asprintf "%a" pp_error e);
          Lwt.return (Error e)
      | Ok events -> (
          match List.find_map pick events with
          | Some v -> Lwt.return (Ok v)
          | None -> await t ~budget:(budget - 1) ~pick)

  let connect ?max_frame ?(max_reads = 1024) ~random flow ~server_pub =
    (* Bound separately and in this order on purpose: argument evaluation
       order is unspecified in OCaml, and a generator's draws should happen in
       a sequence that can be reasoned about and reproduced. *)
    let ephemeral_seed = random 32 in
    let aes_params = random Ton_adnl.Handshake.params_size in
    match Ton_adnl.Conn.connect ?max_frame ~server_pub ~ephemeral_seed ~aes_params () with
    | Error e -> Lwt.return (Error (Client (C.Adnl e)))
    | Ok (conn, packet) -> (
        let t = { flow; session = C.Session.create conn; closed = None; random; max_reads } in
        write t packet >>= function
        | Error e -> Lwt.return (Error e)
        | Ok () -> (
            (* The empty frame the server sends back is the handshake's only
               acknowledgement. *)
            await t ~budget:max_reads ~pick:(fun _ -> Some ()) >|= function
            | Ok () -> Ok t
            | Error e -> Error e))

  let call t q =
    match t.closed with
    | Some m -> Lwt.return (Error (Closed m))
    | None -> (
        let query_id = t.random 32 in
        let nonce = t.random 32 in
        let session, bytes = C.Session.query t.session ~query_id ~nonce q in
        t.session <- session;
        write t bytes >>= function
        | Error e -> Lwt.return (Error e)
        | Ok () -> (
            await t ~budget:t.max_reads ~pick:(function
              | C.Session.Answer a when String.equal a.query_id query_id -> Some a.body
              | _ -> None)
            >|= function
            | Error e -> Error e
            | Ok body -> (
                match C.Query.decode q body with Ok v -> Ok v | Error e -> Error (Client e))))

  let random_int64 t =
    let b = t.random 8 in
    let v = ref 0L in
    String.iter (fun c -> v := Int64.logor (Int64.shift_left !v 8) (Int64.of_int (Char.code c))) b;
    !v

  let ping t =
    match t.closed with
    | Some m -> Lwt.return (Error (Closed m))
    | None -> (
        let random_id = random_int64 t in
        let nonce = t.random 32 in
        let session, bytes = C.Session.ping t.session ~random_id ~nonce in
        t.session <- session;
        write t bytes >>= function
        | Error e -> Lwt.return (Error e)
        | Ok () ->
            await t ~budget:t.max_reads ~pick:(function
              | C.Session.Pong id when Int64.equal id random_id -> Some id
              | _ -> None))

  let close t =
    t.closed <- Some "closed locally";
    F.close t.flow
end
