open Ton_lite_client

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let unhex s =
  String.init (String.length s / 2) (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let transcript =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/adnl-transcript.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "adnl-transcript.json: expected an object")

let field name = match List.assoc_opt name (Lazy.force transcript) with
  | Some v -> v
  | None -> Alcotest.failf "missing field %S" name

let str n = match field n with `String s -> unhex s | _ -> Alcotest.failf "%s: expected a string" n
let strs n = match field n with
  | `List l -> List.map (function `String s -> unhex s | _ -> Alcotest.fail "expected a string") l
  | _ -> Alcotest.failf "%s: expected a list" n

let connect () =
  match
    Ton_adnl.Conn.connect ~server_pub:(str "serverPub") ~ephemeral_seed:(str "ephemeralSeed")
      ~aes_params:(str "aesParams") ()
  with
  | Ok v -> v
  | Error e -> Alcotest.failf "%a" Ton_adnl.Conn.pp_error e

(* Replaying a recorded session is the whole point of keeping the protocol
   free of IO. Every value that would normally be random was fixed when the
   transcript was taken, so the client's side of the conversation is a pure
   function of its inputs and must come out byte for byte identical. *)
let test_replay () =
  let writes = strs "clientWrites" and reads = strs "serverReads" in
  let conn, packet = connect () in
  Alcotest.(check int) "handshake is 256 bytes" 256 (String.length packet);
  Alcotest.(check string) "handshake packet" (hex (List.nth writes 0)) (hex packet);
  let session = Session.create conn in
  let session, query =
    Session.query session ~query_id:(str "queryId") ~nonce:(str "nonce") Query.get_masterchain_info
  in
  Alcotest.(check string) "query frame" (hex (List.nth writes 1)) (hex query);
  (* The server's first frame is empty: that is the handshake confirmation,
     and the only evidence the keys agreed. *)
  Alcotest.(check bool) "not yet confirmed" false (Ton_adnl.Conn.confirmed (Session.conn session));
  let session, events =
    match Session.feed session (List.nth reads 0) with
    | Ok v -> v
    | Error e -> Alcotest.failf "%a" pp_error e
  in
  Alcotest.(check bool) "confirmed" true (Ton_adnl.Conn.confirmed (Session.conn session));
  Alcotest.(check int) "one event" 1 (List.length events);
  (match events with [ Session.Empty ] -> () | _ -> Alcotest.fail "expected an empty frame");
  match Session.feed session (List.nth reads 1) with
  | Error e -> Alcotest.failf "%a" pp_error e
  | Ok (_, [ Session.Answer { query_id; body } ]) ->
      Alcotest.(check string) "answer matches the query id" (hex (str "queryId")) (hex query_id);
      (match Query.decode Query.get_masterchain_info body with
      | Error e -> Alcotest.failf "decoding the answer: %a" pp_error e
      | Ok info ->
          let last = info.Lite.last in
          Alcotest.(check int32) "masterchain workchain" (-1l) last.Lite.workchain;
          Alcotest.(check int64) "masterchain shard" 0x8000000000000000L last.Lite.shard;
          Alcotest.(check bool) "a plausible seqno" true (last.Lite.seqno > 80_000_000l);
          Alcotest.(check int) "root hash is 32 bytes" 32 (String.length last.Lite.root_hash);
          Alcotest.(check int) "file hash is 32 bytes" 32 (String.length last.Lite.file_hash))
  | Ok (_, evs) -> Alcotest.failf "expected one answer, got %d events" (List.length evs)

(* A socket hands over whatever it happens to have. Frames must be
   reassembled the same way regardless of where the chunks fall -- including
   a split inside the encrypted length prefix. *)
let test_arbitrary_chunking () =
  let reads = strs "serverReads" in
  let all = String.concat "" reads in
  let expected =
    let conn, _ = connect () in
    match Session.feed (Session.create conn) all with
    | Ok (_, evs) -> List.length evs
    | Error e -> Alcotest.failf "%a" pp_error e
  in
  Alcotest.(check int) "two frames in the recording" 2 expected;
  let replay chunk_size =
    let conn, _ = connect () in
    let session = ref (Session.create conn) and events = ref [] in
    let i = ref 0 in
    while !i < String.length all do
      let n = min chunk_size (String.length all - !i) in
      (match Session.feed !session (String.sub all !i n) with
      | Ok (s, evs) ->
          session := s;
          events := !events @ evs
      | Error e -> Alcotest.failf "chunk %d: %a" chunk_size pp_error e);
      i := !i + n
    done;
    !events
  in
  List.iter
    (fun size ->
      Alcotest.(check int)
        (Printf.sprintf "%d-byte chunks yield the same frames" size)
        expected (List.length (replay size)))
    [ 1; 2; 3; 7; 63; 64; 65; 67; 68; 100; 359; 360; 1000 ]

(* A frame is only accepted once it is complete; a partial one must be held,
   not guessed at. *)
let test_partial_is_held () =
  let all = String.concat "" (strs "serverReads") in
  let conn, _ = connect () in
  let session = Session.create conn in
  match Session.feed session (String.sub all 0 60) with
  | Ok (s, []) -> Alcotest.(check int) "held back" 60 (Ton_adnl.Conn.buffered (Session.conn s))
  | Ok (_, evs) -> Alcotest.failf "decoded %d frames from a partial one" (List.length evs)
  | Error e -> Alcotest.failf "%a" pp_error e

(* --- queries ---------------------------------------------------------------- *)

(* Get-method identifiers are a CRC-16 of the name with bit 16 set. These are
   the ones every wallet integration uses. *)
let test_method_ids () =
  List.iter
    (fun (name, expected) ->
      Alcotest.(check int64) name expected (method_id name))
    [ ("seqno", 85143L); ("get_public_key", 78748L); ("get_wallet_data", 97026L);
      ("get_jetton_data", 106029L) ];
  (* Independently: the low 16 bits are exactly the CRC. *)
  Alcotest.(check int64) "bit 16 is set" 0x10000L (Int64.logand (method_id "seqno") 0x10000L);
  Alcotest.(check int) "low bits are the crc" (Web3_codec.Crc.crc16_xmodem "seqno")
    (Int64.to_int (Int64.logand (method_id "seqno") 0xffffL))

let test_query_encodings () =
  (* Each query is the boxed method, and nothing else. *)
  Alcotest.(check string) "getMasterchainInfo" "2ee6b589" (hex (Query.encode Query.get_masterchain_info));
  (* Every query is its identifier little-endian and nothing more. *)
  let le id = Ton_tl.Tl.Writer.to_string (fun w -> Ton_tl.Tl.Writer.constructor w id) in
  Alcotest.(check string) "getTime" (hex (le Lite.lite_server_get_time_id)) (hex (Query.encode Query.get_time));
  Alcotest.(check string) "getVersion" (hex (le Lite.lite_server_get_version_id))
    (hex (Query.encode Query.get_version));
  let addr = Result.get_ok (Ton_address.of_raw "0:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76") in
  let a = Query.account_id addr in
  Alcotest.(check int32) "account workchain" 0l a.Lite.workchain;
  Alcotest.(check string) "account id" (hex addr.Ton_address.hash) (hex a.Lite.id)

(* A liteserver answers errors in place of any result, so every decoder has
   to treat the response as a union. *)
let test_server_error () =
  let err =
    Ton_tl.Tl.Writer.to_string (fun w ->
        Lite.write_boxed_lite_server_error w { Lite.code = -400l; message = "not found" })
  in
  match Query.decode Query.get_masterchain_info err with
  | Ok _ -> Alcotest.fail "decoded an error as a result"
  | Error (Server { code; message }) ->
      Alcotest.(check int32) "code" (-400l) code;
      Alcotest.(check string) "message" "not found" message
  | Error e -> Alcotest.failf "wrong error: %a" pp_error e

let test_unexpected_constructor () =
  let bogus = Ton_tl.Tl.Writer.to_string (fun w -> Ton_tl.Tl.Writer.constructor w 0xdeadbeefl) in
  match Query.decode Query.get_masterchain_info bogus with
  | Ok _ -> Alcotest.fail "decoded an unrelated constructor"
  | Error _ -> ()

let () =
  Alcotest.run "lite client"
    [ ( "replay",
        [ Alcotest.test_case "a recorded session, byte for byte" `Quick test_replay;
          Alcotest.test_case "arbitrary chunking" `Quick test_arbitrary_chunking;
          Alcotest.test_case "partial frames are held" `Quick test_partial_is_held ] );
      ( "queries",
        [ Alcotest.test_case "get-method identifiers" `Quick test_method_ids;
          Alcotest.test_case "encodings" `Quick test_query_encodings;
          Alcotest.test_case "server errors" `Quick test_server_error;
          Alcotest.test_case "unexpected constructor" `Quick test_unexpected_constructor ] )
    ]
