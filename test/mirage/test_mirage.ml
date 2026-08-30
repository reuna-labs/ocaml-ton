(* Drives the MirageOS transport over an in-memory flow replaying the
   recorded mainnet session. No socket, no unikernel, no network: the same
   trick that makes ADNL testable at all also makes its transport testable. *)
open Lwt.Infix

let hex s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let unhex s =
  String.init
    (String.length s / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let transcript =
  lazy
    (match
       Yojson.Safe.from_string (read_file "../vectors/adnl-transcript.json")
     with
    | `Assoc l -> l
    | _ -> Alcotest.fail "adnl-transcript.json: expected an object")

let field n =
  match List.assoc_opt n (Lazy.force transcript) with
  | Some v -> v
  | None -> Alcotest.failf "missing field %S" n

let str n =
  match field n with `String s -> unhex s | _ -> Alcotest.failf "%s" n

let strs n =
  match field n with
  | `List l ->
      List.map
        (function
          | `String s -> unhex s | _ -> Alcotest.fail "expected a string")
        l
  | _ -> Alcotest.failf "%s" n

(* A flow that hands back the recorded server bytes and records what the
   client writes, so both directions can be checked. *)
module Replay = struct
  (* This flow never fails; running out of input is reported as `Eof rather
     than as an error, so the error type is only ever a placeholder. *)
  type error = Never [@@warning "-37"]
  type write_error = Mirage_flow.write_error

  let pp_error ppf Never = Fmt.string ppf "unreachable"
  let pp_write_error = Mirage_flow.pp_write_error

  type flow = { mutable reads : string list; mutable writes : string list }

  let make reads = { reads; writes = [] }

  let read f =
    match f.reads with
    | [] -> Lwt.return (Ok `Eof)
    | r :: rest ->
        f.reads <- rest;
        Lwt.return (Ok (`Data (Cstruct.of_string r)))

  let write f cs =
    f.writes <- f.writes @ [ Cstruct.to_string cs ];
    Lwt.return (Ok ())

  let writev f css = write f (Cstruct.concat css)
  let shutdown _ _ = Lwt.return_unit
  let close _ = Lwt.return_unit
end

module T = Ton_lite_client_mirage.Make (Replay)

(* Fixed "randomness", matching what the transcript was taken with, so the
   client's side of the conversation reproduces exactly. *)
let scripted () =
  let queue =
    ref [ str "ephemeralSeed"; str "aesParams"; str "queryId"; str "nonce" ]
  in
  fun n ->
    match !queue with
    | v :: rest when String.length v = n ->
        queue := rest;
        v
    | _ -> String.make n '\x00'

let run f = Lwt_main.run f

(* The handshake completes only when the server's empty frame comes back, and
   the bytes the transport put on the wire must match the recording. *)
let test_handshake () =
  let flow = Replay.make (strs "serverReads") in
  match
    run (T.connect ~random:(scripted ()) flow ~server_pub:(str "serverPub"))
  with
  | Error e -> Alcotest.failf "%a" Ton_lite_client_mirage.pp_error e
  | Ok _ ->
      Alcotest.(check string)
        "the handshake packet"
        (hex (List.nth (strs "clientWrites") 0))
        (hex (List.nth flow.Replay.writes 0))

let test_query () =
  let flow = Replay.make (strs "serverReads") in
  let result =
    run
      ( T.connect ~random:(scripted ()) flow ~server_pub:(str "serverPub")
      >>= function
        | Error e -> Lwt.return (Error e)
        | Ok t -> T.call t Ton_lite_client.Query.get_masterchain_info )
  in
  match result with
  | Error e -> Alcotest.failf "%a" Ton_lite_client_mirage.pp_error e
  | Ok info ->
      Alcotest.(check string)
        "the query frame"
        (hex (List.nth (strs "clientWrites") 1))
        (hex (List.nth flow.Replay.writes 1));
      let last = info.Ton_lite_client.Lite.last in
      Alcotest.(check int32)
        "masterchain workchain" (-1l) last.Ton_lite_client.Lite.workchain;
      Alcotest.(check bool)
        "a plausible seqno" true
        (last.Ton_lite_client.Lite.seqno > 80_000_000l)

(* A flow that ends early must surface as a closed connection rather than
   hanging or being mistaken for a timeout. *)
let test_eof () =
  let flow = Replay.make [] in
  match
    run (T.connect ~random:(scripted ()) flow ~server_pub:(str "serverPub"))
  with
  | Ok _ -> Alcotest.fail "completed a handshake with no reply"
  | Error e ->
      Alcotest.(check string)
        "reported" "connection closed: peer closed the connection"
        (Format.asprintf "%a" Ton_lite_client_mirage.pp_error e)

(* Bytes arriving in small pieces must be reassembled, since a flow may
   deliver a frame across many reads. *)
let test_dribble () =
  let all = String.concat "" (strs "serverReads") in
  let pieces = List.init (String.length all) (fun i -> String.sub all i 1) in
  let flow = Replay.make pieces in
  let result =
    run
      ( T.connect ~random:(scripted ()) flow ~server_pub:(str "serverPub")
      >>= function
        | Error e -> Lwt.return (Error e)
        | Ok t -> T.call t Ton_lite_client.Query.get_masterchain_info )
  in
  match result with
  | Error e -> Alcotest.failf "%a" Ton_lite_client_mirage.pp_error e
  | Ok info ->
      Alcotest.(check bool)
        "still decoded" true
        (info.Ton_lite_client.Lite.last.Ton_lite_client.Lite.seqno > 80_000_000l)

let () =
  Alcotest.run "mirage transport"
    [
      ( "replay",
        [
          Alcotest.test_case "handshake" `Quick test_handshake;
          Alcotest.test_case "query" `Quick test_query;
          Alcotest.test_case "one byte at a time" `Quick test_dribble;
        ] );
      ("errors", [ Alcotest.test_case "peer closes early" `Quick test_eof ]);
    ]
