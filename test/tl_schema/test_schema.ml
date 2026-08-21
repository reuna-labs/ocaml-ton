open Ton_tl_schema
module R = Ton_tl.Tl.Reader
module W = Ton_tl.Tl.Writer

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let enc f = W.to_string f
let dec s f = match R.parse s f with Ok v -> v | Error e -> Alcotest.failf "%a" R.pp_error e

(* --- identifiers reach the wire correctly ---------------------------------- *)

(* A boxed value is its identifier followed by its fields, and the identifier
   goes on the wire little-endian. Getting that backwards is the classic way
   to have a liteserver close the connection without a word. *)
let test_boxed_prefix () =
  Alcotest.(check string) "getMasterchainInfo boxed" "2ee6b589"
    (hex (enc (fun w -> Lite.write_boxed_lite_server_get_masterchain_info w ())));
  Alcotest.(check string) "the identifier itself" "89b5e62e"
    (Printf.sprintf "%08lx" Lite.lite_server_get_masterchain_info_id);
  Alcotest.(check string) "tcp.ping boxed" "9a2b084d"
    (hex (String.sub (enc (fun w -> Adnl.write_boxed_tcp_ping w { Adnl.random_id = 0L })) 0 4))

(* The real nesting a liteserver query travels in: an ADNL message carrying a
   liteServer.query, itself carrying the method. Each layer is boxed and each
   bytes field is padded to a multiple of four. *)
let test_query_nesting () =
  let method_ = enc (fun w -> Lite.write_boxed_lite_server_get_masterchain_info w ()) in
  let inner = enc (fun w -> Lite.write_boxed_lite_server_query w { Lite.data = method_ }) in
  Alcotest.(check string) "liteServer.query layer" "df068c79042ee6b589000000" (hex inner);
  let query_id = String.make 32 '\x11' in
  let outer = enc (fun w -> Lite.write_boxed_adnl_message_query w { Lite.query_id; query = inner }) in
  Alcotest.(check string) "adnl.message.query identifier" "7af98bb4" (hex (String.sub outer 0 4));
  Alcotest.(check string) "then the query id" (hex query_id) (hex (String.sub outer 4 32));
  Alcotest.(check int) "and the whole thing is 4-byte aligned" 0 (String.length outer mod 4);
  (* And it decodes back. *)
  let back = dec outer (fun r -> Lite.read_boxed_adnl_message_query r) in
  Alcotest.(check string) "query id survives" (hex query_id) (hex back.Lite.query_id);
  let inner_back = dec back.Lite.query (fun r -> Lite.read_boxed_lite_server_query r) in
  Alcotest.(check string) "inner method survives" (hex method_) (hex inner_back.Lite.data)

(* --- round trips ------------------------------------------------------------ *)

let block_id_ext : Lite.ton_node_block_id_ext =
  { workchain = -1l; shard = 0x8000000000000000L; seqno = 42l;
    root_hash = String.make 32 '\x01'; file_hash = String.make 32 '\x02' }

let test_block_id_ext () =
  let e = enc (fun w -> Lite.write_boxed_ton_node_block_id_ext w block_id_ext) in
  let back = dec e (fun r -> Lite.read_boxed_ton_node_block_id_ext r) in
  Alcotest.(check int32) "workchain" (-1l) back.Lite.workchain;
  Alcotest.(check int64) "shard" 0x8000000000000000L back.Lite.shard;
  Alcotest.(check int32) "seqno" 42l back.Lite.seqno;
  Alcotest.(check string) "root hash" (hex block_id_ext.Lite.root_hash) (hex back.Lite.root_hash)

(* runMethodResult carries five independently optional fields keyed off one
   flags word, and they are not read in bit order. Every combination has to
   survive, or a result would decode as garbage for some modes only. *)
let test_conditional_fields () =
  for m = 0 to 31 do
    let mode = Int32.of_int m in
    let opt bit v = if m land (1 lsl bit) <> 0 then Some v else None in
    let v : Lite.lite_server_run_method_result =
      { mode; id = block_id_ext; shardblk = block_id_ext;
        shard_proof = opt 0 "sp"; proof = opt 0 "pr"; state_proof = opt 1 "st";
        init_c7 = opt 3 "c7"; lib_extras = opt 4 "lx"; exit_code = 7l; result = opt 2 "rs" }
    in
    let back = dec (enc (fun w -> Lite.write_boxed_lite_server_run_method_result w v))
                 (fun r -> Lite.read_boxed_lite_server_run_method_result r)
    in
    let at f = Printf.sprintf "mode %d: %s" m f in
    Alcotest.(check int32) (at "mode") mode back.Lite.mode;
    Alcotest.(check int32) (at "exit code") 7l back.Lite.exit_code;
    Alcotest.(check (option string)) (at "shard_proof") (opt 0 "sp") back.Lite.shard_proof;
    Alcotest.(check (option string)) (at "proof") (opt 0 "pr") back.Lite.proof;
    Alcotest.(check (option string)) (at "state_proof") (opt 1 "st") back.Lite.state_proof;
    Alcotest.(check (option string)) (at "result") (opt 2 "rs") back.Lite.result;
    Alcotest.(check (option string)) (at "init_c7") (opt 3 "c7") back.Lite.init_c7;
    Alcotest.(check (option string)) (at "lib_extras") (opt 4 "lx") back.Lite.lib_extras
  done

(* A field present while its flag bit is clear would simply not be written,
   and the peer would then misparse everything after it. The generated writer
   refuses instead. *)
let test_flag_mismatch_is_caught () =
  let v : Lite.lite_server_run_method_result =
    { mode = 0l; id = block_id_ext; shardblk = block_id_ext;
      shard_proof = Some "oops"; proof = None; state_proof = None; init_c7 = None;
      lib_extras = None; exit_code = 0l; result = None }
  in
  match enc (fun w -> Lite.write_boxed_lite_server_run_method_result w v) with
  | _ -> Alcotest.fail "wrote a field whose flag bit was clear"
  | exception Invalid_argument m ->
      Alcotest.(check string) "message" "mode.0 is false but the field is true" m

(* mode.N?true fields are the flag bit itself. *)
let test_true_fields () =
  (* Annotated because listBlockTransactions and its Ext sibling share every
     field name; without it the record literal picks whichever was defined
     last. The generated code annotates for the same reason. *)
  let v : Lite.lite_server_list_block_transactions =
    { id = block_id_ext; mode = 0x60l (* bits 5 and 6 *); count = 10l;
      after = None; reverse_order = true; want_proof = true }
  in
  let back = dec (enc (fun w -> Lite.write_boxed_lite_server_list_block_transactions w v))
               (fun r -> Lite.read_boxed_lite_server_list_block_transactions r)
  in
  Alcotest.(check bool) "reverse_order" true back.Lite.reverse_order;
  Alcotest.(check bool) "want_proof" true back.Lite.want_proof;
  Alcotest.(check bool) "after is absent" true (back.Lite.after = None)

(* Result types with several constructors dispatch on the identifier. *)
let test_variant_dispatch () =
  let sig_ : Lite.lite_server_signature = { node_id_short = String.make 32 '\x03'; signature = "sig" } in
  let ordinary =
    Lite.Ordinary { Lite.validator_set_hash = 1l; catchain_seqno = 2l; signatures = [ sig_ ] }
  in
  let back = dec (enc (fun w -> Lite.write_boxed_lite_server_signature_set w ordinary))
               (fun r -> Lite.read_boxed_lite_server_signature_set r)
  in
  (match back with
  | Lite.Ordinary o ->
      Alcotest.(check int32) "validator set hash" 1l o.Lite.validator_set_hash;
      Alcotest.(check int) "one signature" 1 (List.length o.Lite.signatures)
  | Lite.Simplex _ -> Alcotest.fail "decoded the wrong constructor");
  let simplex =
    Lite.Simplex
      { Lite.cc_seqno = 9l; validator_set_hash = 8l; signatures = []; session_id = String.make 32 '\x04';
        slot = 3l; candidate = "cand" }
  in
  match dec (enc (fun w -> Lite.write_boxed_lite_server_signature_set w simplex))
          (fun r -> Lite.read_boxed_lite_server_signature_set r)
  with
  | Lite.Simplex s -> Alcotest.(check int32) "cc seqno" 9l s.Lite.cc_seqno
  | Lite.Ordinary _ -> Alcotest.fail "decoded the wrong constructor"

let test_unknown_constructor () =
  match R.parse (enc (fun w -> W.constructor w 0xdeadbeefl)) (fun r -> Lite.read_boxed_lite_server_signature_set r) with
  | Ok _ -> Alcotest.fail "accepted an unknown constructor"
  | Error e ->
      Alcotest.(check string) "error" "unexpected constructor deadbeef for liteServer.SignatureSet"
        (Format.asprintf "%a" R.pp_error e)

(* Vectors of bare constructors are written without identifiers; vectors of
   boxed ones carry one per element. *)
let test_vectors_of_constructors () =
  let v : Lite.lite_server_transaction_list = { ids = [ block_id_ext; block_id_ext ]; transactions = "tx" } in
  let back = dec (enc (fun w -> Lite.write_boxed_lite_server_transaction_list w v))
               (fun r -> Lite.read_boxed_lite_server_transaction_list r)
  in
  Alcotest.(check int) "two ids" 2 (List.length back.Lite.ids);
  Alcotest.(check string) "transactions" "tx" back.Lite.transactions

let () =
  Alcotest.run "tl_schema"
    [ ( "identifiers",
        [ Alcotest.test_case "boxed prefix" `Quick test_boxed_prefix;
          Alcotest.test_case "query nesting" `Quick test_query_nesting ] );
      ( "round trips",
        [ Alcotest.test_case "blockIdExt" `Quick test_block_id_ext;
          Alcotest.test_case "conditional fields, all 32 modes" `Quick test_conditional_fields;
          Alcotest.test_case "mode.N?true fields" `Quick test_true_fields;
          Alcotest.test_case "vectors of constructors" `Quick test_vectors_of_constructors ] );
      ( "dispatch",
        [ Alcotest.test_case "variant constructors" `Quick test_variant_dispatch;
          Alcotest.test_case "unknown constructor" `Quick test_unknown_constructor;
          Alcotest.test_case "flag mismatch is refused" `Quick test_flag_mismatch_is_caught ] )
    ]
