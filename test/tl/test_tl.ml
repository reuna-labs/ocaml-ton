open Ton_tl.Tl

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let vectors =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/tl-expected.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "tl-expected.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_bool = function `Bool b -> b | _ -> Alcotest.fail "expected a bool"
let to_int = function `Int n -> n | `Intlit s -> int_of_string s | _ -> Alcotest.fail "expected an int"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"

(* --- identifiers ----------------------------------------------------------- *)

(* Published values, transcribed from independent implementations and from
   the TON documentation. These are the anchors: they pin the rule itself,
   not merely our consistency with a second copy of our own reading of it. *)
let published =
  [ ("pub.ed25519 key:int256 = PublicKey", 0x4813b4c6l);
    ("adnl.message.query query_id:int256 query:bytes = adnl.Message", 0xb48bf97al);
    ("adnl.message.answer query_id:int256 answer:bytes = adnl.Message", 0x0fac8416l);
    ("liteServer.query data:bytes = Object", 0x798c06dfl);
    ("liteServer.getMasterchainInfo = liteServer.MasterchainInfo", 0x89b5e62el);
    ( "liteServer.getAccountState id:tonNode.blockIdExt account:liteServer.accountId = liteServer.AccountState",
      0x6b890e25l );
    ("liteServer.sendMessage body:bytes = liteServer.SendMsgStatus", 0x690ad482l);
    ("liteServer.error code:int message:string = liteServer.Error", 0xbba9e148l);
    ("tcp.ping random_id:long = tcp.Pong", 0x4d082b9al);
    ("tcp.pong random_id:long = tcp.Pong", 0xdc69fb03l);
    ("liteServer.waitMasterchainSeqno seqno:int timeout_ms:int = Object", 0xbaeab892l);
    ( "tonNode.blockIdExt workchain:int shard:long seqno:int root_hash:int256 file_hash:int256 = tonNode.BlockIdExt",
      0x6752eb78l );
    ("boolTrue = Bool", 0x997275b5l);
    ("boolFalse = Bool", 0xbc799737l);
    ( "liteServer.runSmcMethod mode:# id:tonNode.blockIdExt account:liteServer.accountId method_id:long params:bytes = liteServer.RunMethodResult",
      0x5cc65dd2l ) ]

let test_published () =
  List.iter
    (fun (line, want) ->
      Alcotest.(check string)
        (Printf.sprintf "%s..." (String.sub line 0 (min 40 (String.length line))))
        (Printf.sprintf "%08lx" want)
        (Printf.sprintf "%08lx" (Crc.id_of_definition line)))
    published

(* Every definition in both vendored schemas, cross-checked against ids
   derived by a separate implementation of the same rule. The source lines
   come from that implementation's parser too, so a disagreement about where
   a definition begins or ends shows up here rather than silently. *)
let test_all_definitions () =
  let total = ref 0 and explicit = ref 0 in
  List.iter
    (fun (file, defs) ->
      List.iter
        (fun (name, spec) ->
          let spec = to_obj spec in
          incr total;
          if to_bool (field "explicit" spec) then incr explicit;
          let source = to_str (field "source" spec) in
          Alcotest.(check string)
            (Printf.sprintf "%s: %s" file name)
            (Printf.sprintf "%08lx" (Int32.of_int (to_int (field "id" spec) land 0xffffffff)))
            (Printf.sprintf "%08lx" (Crc.id_of_definition source)))
        (to_obj defs))
    (Lazy.force vectors);
  Alcotest.(check bool) "checked the whole schema" true (!total > 700);
  Alcotest.(check bool) "and some ids are pinned rather than derived" true (!explicit > 0)

(* A pinned identifier is authoritative and does not agree with the hash of
   its own text -- which is exactly why it has to be honoured rather than
   recomputed. *)
let test_explicit_overrides () =
  let line =
    "liteServer.transactionId#b12f65af mode:# account:mode.0?int256 lt:mode.1?long hash:mode.2?int256 metadata:mode.8?liteServer.transactionMetadata = liteServer.TransactionId;"
  in
  Alcotest.(check (option string)) "explicit id is read out" (Some "b12f65af")
    (Option.map (Printf.sprintf "%08lx") (Crc.explicit_id line));
  Alcotest.(check string) "and is what goes on the wire" "b12f65af"
    (Printf.sprintf "%08lx" (Crc.id_of_definition line));
  Alcotest.(check bool) "which differs from the computed value" false
    (Crc.constructor_id line = Crc.id_of_definition line);
  Alcotest.(check (option string)) "no id where none is written" None
    (Option.map (Printf.sprintf "%08lx") (Crc.explicit_id "liteServer.getMasterchainInfo = liteServer.MasterchainInfo"))

let test_normalize () =
  Alcotest.(check string) "collapses whitespace and drops punctuation"
    "liteServer.getLibraries library_list:vector int256 = liteServer.LibraryResult"
    (Crc.normalize "liteServer.getLibraries   library_list:(vector int256)  = liteServer.LibraryResult; // note");
  Alcotest.(check string) "multi-line definitions join cleanly"
    "a b c = D" (Crc.normalize "a b\n    c = D;")

(* --- wire format ----------------------------------------------------------- *)

let enc f = Writer.to_string f
let dec s f = match Reader.parse s f with Ok v -> v | Error e -> Alcotest.failf "%a" Reader.pp_error e

let test_ints () =
  Alcotest.(check string) "int is little-endian" "78563412" (hex (enc (fun w -> Writer.int w 0x12345678l)));
  Alcotest.(check string) "negative int" "ffffffff" (hex (enc (fun w -> Writer.int w (-1l))));
  Alcotest.(check string) "long is little-endian" "efcdab8967452301"
    (hex (enc (fun w -> Writer.long w 0x0123456789abcdefL)));
  Alcotest.(check int32) "int roundtrip" (-42l) (dec (enc (fun w -> Writer.int w (-42l))) Reader.int);
  Alcotest.(check int64) "long roundtrip" (-42L) (dec (enc (fun w -> Writer.long w (-42L))) Reader.long);
  Alcotest.(check bool) "bool true" true (dec (enc (fun w -> Writer.bool w true)) Reader.bool);
  Alcotest.(check bool) "bool false" false (dec (enc (fun w -> Writer.bool w false)) Reader.bool);
  (* Booleans are boxed, so they cost a whole constructor. *)
  Alcotest.(check string) "boolTrue on the wire" "b5757299" (hex (enc (fun w -> Writer.bool w true)))

(* The length prefix has three forms and the whole field is padded to a
   multiple of four. The boundaries are where implementations disagree. *)
let test_bytes_boundaries () =
  List.iter
    (fun len ->
      let s = String.make len 'x' in
      let e = enc (fun w -> Writer.bytes w s) in
      Alcotest.(check int)
        (Printf.sprintf "length %d is a multiple of 4" len)
        0 (String.length e mod 4);
      let expect_prefix = if len < 254 then 1 else if len < 0x1000000 then 4 else 8 in
      Alcotest.(check int)
        (Printf.sprintf "length %d uses the right prefix" len)
        expect_prefix
        (String.length e - len - ((4 - ((expect_prefix + len) mod 4)) mod 4));
      Alcotest.(check string) (Printf.sprintf "length %d roundtrips" len) s (dec e Reader.bytes))
    [ 0; 1; 2; 3; 4; 252; 253; 254; 255; 256; 1000 ]

let test_bytes_shapes () =
  Alcotest.(check string) "empty is one length byte and three pad" "00000000"
    (hex (enc (fun w -> Writer.bytes w "")));
  Alcotest.(check string) "one byte" "01610000" (hex (enc (fun w -> Writer.bytes w "a")));
  Alcotest.(check string) "three bytes need no padding" "03616263" (hex (enc (fun w -> Writer.bytes w "abc")));
  (* 254 switches to the escape form: 0xFE then a 3-byte little-endian length *)
  let e = enc (fun w -> Writer.bytes w (String.make 254 'x')) in
  Alcotest.(check string) "the long form prefix" "fefe0000" (hex (String.sub e 0 4))

let test_vector () =
  let e = enc (fun w -> Writer.vector w Writer.int [ 1l; 2l; 3l ]) in
  Alcotest.(check string) "count then elements" "03000000010000000200000003000000" (hex e);
  Alcotest.(check (list int32)) "roundtrip" [ 1l; 2l; 3l ] (dec e (fun r -> Reader.vector r Reader.int));
  Alcotest.(check (list int32)) "empty" [] (dec (enc (fun w -> Writer.vector w Writer.int [])) (fun r -> Reader.vector r Reader.int))

let test_rejections () =
  let err f s = match Reader.parse s f with
    | Ok _ -> Alcotest.fail "accepted malformed input"
    | Error e -> Format.asprintf "%a" Reader.pp_error e
  in
  Alcotest.(check string) "truncated int" "truncated reading int: want 4 bytes, have 2" (err Reader.int "ab");
  Alcotest.(check string) "unexpected constructor" "expected constructor 00000001, got 12345678"
    (err (fun r -> Reader.expect r 1l) (enc (fun w -> Writer.int w 0x12345678l)));
  Alcotest.(check string) "not a boolean" "expected a boolean constructor, got 00000000"
    (err Reader.bool "\000\000\000\000");
  Alcotest.(check string) "non-zero padding" "non-zero padding"
    (err Reader.bytes "\001a\001\000");
  (* A vector claiming more elements than could possibly follow. *)
  Alcotest.(check string) "implausible vector" "implausible vector length 16777216"
    (err (fun r -> Reader.vector r Reader.int) "\000\000\000\001")

let test_finish () =
  match Reader.parse (enc (fun w -> Writer.int w 1l) ^ "junk") (fun r -> ignore (Reader.int r); Reader.finish r) with
  | Ok () -> Alcotest.fail "accepted trailing bytes"
  | Error e -> Alcotest.(check string) "error" "4 trailing bytes" (Format.asprintf "%a" Reader.pp_error e)

(* --- properties ------------------------------------------------------------ *)

let prop_bytes =
  QCheck2.Test.make ~count:2000 ~name:"bytes roundtrip and stay 4-byte aligned"
    QCheck2.Gen.(string_size (int_range 0 600))
    (fun s ->
      let e = enc (fun w -> Writer.bytes w s) in
      String.length e mod 4 = 0 && dec e Reader.bytes = s)

let prop_int =
  QCheck2.Test.make ~count:2000 ~name:"int roundtrip" QCheck2.Gen.int32 (fun v ->
      dec (enc (fun w -> Writer.int w v)) Reader.int = v)

let prop_long =
  QCheck2.Test.make ~count:2000 ~name:"long roundtrip" QCheck2.Gen.int64 (fun v ->
      dec (enc (fun w -> Writer.long w v)) Reader.long = v)

let prop_vector =
  QCheck2.Test.make ~count:500 ~name:"vector of bytes roundtrip"
    QCheck2.Gen.(list_size (int_range 0 20) (string_size (int_range 0 300)))
    (fun l ->
      dec (enc (fun w -> Writer.vector w Writer.bytes l)) (fun r -> Reader.vector r Reader.bytes) = l)

let () =
  Alcotest.run "tl"
    [ ( "identifiers",
        [ Alcotest.test_case "published values" `Quick test_published;
          Alcotest.test_case "every definition in both schemas" `Quick test_all_definitions;
          Alcotest.test_case "pinned ids override" `Quick test_explicit_overrides;
          Alcotest.test_case "normalisation" `Quick test_normalize ] );
      ( "wire format",
        [ Alcotest.test_case "integers" `Quick test_ints;
          Alcotest.test_case "bytes length boundaries" `Quick test_bytes_boundaries;
          Alcotest.test_case "bytes shapes" `Quick test_bytes_shapes;
          Alcotest.test_case "vectors" `Quick test_vector;
          Alcotest.test_case "rejections" `Quick test_rejections;
          Alcotest.test_case "trailing input" `Quick test_finish ] );
      ( "properties",
        List.map (QCheck_alcotest.to_alcotest ~verbose:false) [ prop_bytes; prop_int; prop_long; prop_vector ] )
    ]
