open Lwt.Infix
module C = Ton_lite_client

type error = Client of C.error | Closed of string | Timeout

let pp_error ppf = function
  | Client e -> C.pp_error ppf e
  | Closed m -> Format.fprintf ppf "connection closed: %s" m
  | Timeout -> Format.pp_print_string ppf "timed out"

type t = {
  fd : Lwt_unix.file_descr;
  mutable session : C.Session.t;
  (* Answers are matched to queries by the 32-byte identifier the request
     carried; nothing about the wire says which query an answer belongs to. *)
  pending : (string, string Lwt.u) Hashtbl.t;
  mutable pongs : int64 Lwt.u list;
  mutable closed : string option;
  mutable reader : unit Lwt.t;
}

let random n = Mirage_crypto_rng.generate n
let read_buf = 65536

let fail_all t reason =
  t.closed <- Some reason;
  Hashtbl.iter (fun _ u -> try Lwt.wakeup_later_exn u (Failure reason) with _ -> ()) t.pending;
  Hashtbl.reset t.pending;
  List.iter (fun u -> try Lwt.wakeup_later_exn u (Failure reason) with _ -> ()) t.pongs;
  t.pongs <- []

let dispatch t = function
  | C.Session.Empty -> ()
  | C.Session.Pong id -> (
      match t.pongs with
      | [] -> ()
      | u :: rest ->
          t.pongs <- rest;
          Lwt.wakeup_later u id)
  | C.Session.Answer { query_id; body } -> (
      match Hashtbl.find_opt t.pending query_id with
      | None -> () (* an answer to a query we are no longer waiting on *)
      | Some u ->
          Hashtbl.remove t.pending query_id;
          Lwt.wakeup_later u body)

(* One reader for the connection: frames arrive in whatever chunks the socket
   hands over, and the session reassembles them. *)
let rec read_loop t =
  Lwt.catch
    (fun () ->
      let buf = Bytes.create read_buf in
      Lwt_unix.read t.fd buf 0 read_buf >>= fun n ->
      if n = 0 then (fail_all t "peer closed the connection"; Lwt.return_unit)
      else
        match C.Session.feed t.session (Bytes.sub_string buf 0 n) with
        | Error e ->
            fail_all t (Format.asprintf "%a" C.pp_error e);
            Lwt.return_unit
        | Ok (session, events) ->
            t.session <- session;
            List.iter (dispatch t) events;
            read_loop t)
    (fun exn ->
      fail_all t (Printexc.to_string exn);
      Lwt.return_unit)

let write_all fd s =
  let rec go off =
    if off >= String.length s then Lwt.return_unit
    else
      Lwt_unix.write_string fd s off (String.length s - off) >>= fun n ->
      if n = 0 then Lwt.fail (Failure "short write") else go (off + n)
  in
  go 0

let connect ?(timeout = 10.0) ?max_frame ~host ~port ~server_pub () =
  Lwt.catch
    (fun () ->
      let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
      let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt_unix.setsockopt fd Unix.TCP_NODELAY true;
      Lwt_unix.connect fd addr >>= fun () ->
      match
        Ton_adnl.Conn.connect ?max_frame ~server_pub ~ephemeral_seed:(random 32)
          ~aes_params:(random Ton_adnl.Handshake.params_size) ()
      with
      | Error e -> Lwt_unix.close fd >|= fun () -> Error (Client (C.Adnl e))
      | Ok (conn, packet) ->
          let t =
            { fd; session = C.Session.create conn; pending = Hashtbl.create 8; pongs = [];
              closed = None; reader = Lwt.return_unit }
          in
          write_all fd packet >>= fun () ->
          t.reader <- read_loop t;
          (* The server proves it derived the same session keys by answering
             with an empty frame. Nothing else acknowledges the handshake, so
             a wrong key looks exactly like a slow server. *)
          let rec wait n =
            if Ton_adnl.Conn.confirmed (C.Session.conn t.session) then Lwt.return_true
            else if n <= 0 || t.closed <> None then Lwt.return_false
            else Lwt_unix.sleep 0.01 >>= fun () -> wait (n - 1)
          in
          wait (int_of_float (timeout /. 0.01)) >>= fun ok ->
          if ok then Lwt.return (Ok t)
          else
            Lwt_unix.close fd >|= fun () ->
            Error (Closed (match t.closed with Some m -> m | None -> "handshake not confirmed")))
    (fun exn -> Lwt.return (Error (Closed (Printexc.to_string exn))))

(* One helper for "write these bytes and wait for the matching reply", used
   by both queries and pings. They differ only in how a reply is recognised,
   so the waiter type is a parameter rather than something cast. *)
let await ~timeout t ~register ~bytes =
  match t.closed with
  | Some m -> Lwt.return (Error (Closed m))
  | None ->
      let waiter, u = Lwt.wait () in
      register u;
      Lwt.catch
        (fun () ->
          write_all t.fd bytes >>= fun () ->
          Lwt.pick [ (waiter >|= fun v -> Ok v); (Lwt_unix.sleep timeout >|= fun () -> Error Timeout) ])
        (fun exn -> Lwt.return (Error (Closed (Printexc.to_string exn))))

let call ?(timeout = 30.0) t q =
  let query_id = random 32 in
  let session, bytes = C.Session.query t.session ~query_id ~nonce:(random 32) q in
  t.session <- session;
  await ~timeout t ~bytes ~register:(fun u -> Hashtbl.replace t.pending query_id u) >|= function
  | Error e -> Error e
  | Ok body -> ( match C.Query.decode q body with Ok v -> Ok v | Error e -> Error (Client e))

let random_int64 () =
  let b = random 8 in
  let v = ref 0L in
  String.iter (fun c -> v := Int64.logor (Int64.shift_left !v 8) (Int64.of_int (Char.code c))) b;
  !v

let ping ?(timeout = 30.0) t =
  let random_id = random_int64 () in
  let session, bytes = C.Session.ping t.session ~random_id ~nonce:(random 32) in
  t.session <- session;
  await ~timeout t ~bytes ~register:(fun u -> t.pongs <- t.pongs @ [ u ])

let close t =
  t.closed <- Some "closed locally";
  Lwt.catch (fun () -> Lwt_unix.close t.fd) (fun _ -> Lwt.return_unit)

let with_connection ?timeout ?max_frame ~host ~port ~server_pub f =
  connect ?timeout ?max_frame ~host ~port ~server_pub () >>= function
  | Error e -> Lwt.return (Error e)
  | Ok t -> Lwt.finalize (fun () -> f t) (fun () -> close t)
