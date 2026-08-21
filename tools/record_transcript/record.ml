(* Records a real ADNL session as a transcript that can be replayed offline.
   Not part of the build's test path; run by hand when a new transcript is
   wanted.

   Every input that would normally be random -- the ephemeral key, the session
   parameters, the query identifier, each frame nonce -- is fixed here, which
   is what makes the client's side of the conversation reproducible. The
   server's side is whatever it said at the time, frozen. *)
open Lwt.Infix
module C = Ton_lite_client

let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))
let fixed byte n = String.make n (Char.chr byte)

let write_all fd s =
  let rec go off =
    if off >= String.length s then Lwt.return_unit
    else Lwt_unix.write_string fd s off (String.length s - off) >>= fun n -> go (off + n)
  in
  go 0

let read_some fd =
  let buf = Bytes.create 65536 in
  Lwt_unix.read fd buf 0 65536 >|= fun n -> Bytes.sub_string buf 0 n

let () =
  let host = if Array.length Sys.argv > 1 then Sys.argv.(1) else "5.9.10.47" in
  let port = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 19949 in
  let key =
    if Array.length Sys.argv > 3 then Sys.argv.(3) else "n4VDnSCUuSpjnCyUk9e3QOOd6o0ItSWYbTnW3Wnn8wk="
  in
  let server_pub = Result.get_ok (Base64.decode key) in
  let ephemeral_seed = fixed 0x5a 32 in
  let aes_params = String.init 160 (fun i -> Char.chr ((0x31 + i) land 0xff)) in
  let query_id = fixed 0x7c 32 in
  let nonce = fixed 0x2d 32 in
  let writes = ref [] and reads = ref [] in
  Lwt_main.run
    (let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
     Lwt_unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_of_string host, port)) >>= fun () ->
     let conn, packet =
       Result.get_ok (Ton_adnl.Conn.connect ~server_pub ~ephemeral_seed ~aes_params ())
     in
     writes := [ packet ];
     write_all fd packet >>= fun () ->
     let session = C.Session.create conn in
     let session, bytes = C.Session.query session ~query_id ~nonce C.Query.get_masterchain_info in
     writes := !writes @ [ bytes ];
     write_all fd bytes >>= fun () ->
     (* Read until the answer arrives. *)
     let rec loop session answers =
       if answers >= 1 then Lwt.return session
       else
         read_some fd >>= fun data ->
         if data = "" then Lwt.return session
         else begin
           reads := !reads @ [ data ];
           match C.Session.feed session data with
           | Error e -> failwith (Format.asprintf "%a" C.pp_error e)
           | Ok (session, events) ->
               let got = List.length (List.filter (function C.Session.Answer _ -> true | _ -> false) events) in
               loop session (answers + got)
         end
     in
     loop session 0 >>= fun _ ->
     Lwt_unix.close fd)
  ;
  let json =
    Printf.sprintf
      {|{
 "note": "A real ADNL session with every client-side random value fixed, so the client's writes are reproducible. The server's reads are frozen as recorded.",
 "host": "%s",
 "port": %d,
 "serverPub": "%s",
 "ephemeralSeed": "%s",
 "aesParams": "%s",
 "queryId": "%s",
 "nonce": "%s",
 "clientWrites": [%s],
 "serverReads": [%s]
}
|}
      host port (hex server_pub) (hex ephemeral_seed) (hex aes_params) (hex query_id) (hex nonce)
      (String.concat ", " (List.map (fun s -> Printf.sprintf "\"%s\"" (hex s)) !writes))
      (String.concat ", " (List.map (fun s -> Printf.sprintf "\"%s\"" (hex s)) !reads))
  in
  print_string json
