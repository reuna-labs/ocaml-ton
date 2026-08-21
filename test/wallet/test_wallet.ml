open Ton_cell
open Ton_wallet
open Wallet

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
    (match Yojson.Safe.from_string (read_file "../vectors/wallet-expected.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "wallet-expected.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_int = function `Int n -> n | _ -> Alcotest.fail "expected an int"
let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"
let to_list = function `List l -> l | _ -> Alcotest.fail "expected a list"
let section n = field n (Lazy.force vectors)

let key =
  lazy
    (let words = List.map to_str (to_list (field "mnemonic" (to_obj (section "key")))) in
     match Ton_crypto.Mnemonic.to_keypair words with
     | Ok k -> k
     | Error e -> Alcotest.failf "mnemonic: %s" e)

let ok what = function Ok v -> v | Error e -> Alcotest.failf "%s: %s" what e

let dest = Result.get_ok (Ton_address.of_raw "0:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76")
let dest2 = Result.get_ok (Ton_address.of_raw "-1:34517c7bdf5187c55af4f8b61fdc321588c7ab768dee24b006df29106458d7cf")
let nano s = Z.of_string s
let cell_of b = match Builder.end_cell b with Ok c -> c | Error e -> Alcotest.failf "%a" Builder.pp_error e

(* The same three transfers the generator builds. *)
let transfers =
  [ ( "single",
      [ internal ~bounce:true ~dest ~value:(nano "1500000000")
          ~body:(cell_of (Builder.store_bytes (Builder.store_uint (Builder.create ()) 0L ~bits:32) "hi"))
          () ] );
    ("no_body", [ internal ~bounce:false ~dest ~value:(nano "50000000") () ]);
    ( "two",
      [ internal ~bounce:true ~dest ~value:(nano "1000000000") ();
        internal ~bounce:false ~dest:dest2 ~value:(nano "2000000000") () ] ) ]

(* Each vector name spells out the configuration it was generated with. An
   explicit table beats parsing the name. *)
let configs =
  [ ("v3r2_wc0", (V3R2, Mainnet, 0, 0));
    ("v3r2_wc-1", (V3R2, Mainnet, -1, 0));
    ("v4r2_wc0", (V4R2, Mainnet, 0, 0));
    ("v4r2_wc-1", (V4R2, Mainnet, -1, 0));
    ("v5r1_wc0", (V5R1, Mainnet, 0, 0));
    ("v5r1_wc-1", (V5R1, Mainnet, -1, 0));
    ("v5r1_testnet_wc0", (V5R1, Testnet, 0, 0));
    ("v5r1_subwallet7", (V5R1, Mainnet, 0, 7)) ]

let wallet_case name spec () =
  let spec = to_obj spec in
  let v, network, workchain, subwallet =
    match List.assoc_opt name configs with
    | Some c -> c
    | None -> Alcotest.failf "no configuration recorded for vector %S" name
  in
  let w =
    ok name (create ~workchain ~network ~subwallet v ~public_key:(Ton_crypto.Ed25519.public (Lazy.force key)))
  in
  let at f = Printf.sprintf "%s: %s" name f in
  Alcotest.(check int) (at "workchain") (to_int (field "workchain" spec)) (Wallet.workchain w);
  Alcotest.(check int32) (at "wallet id")
    (Int32.of_int (to_int (field name (to_obj (section "walletIds")))))
    (Wallet.wallet_id w);
  let si = Wallet.state_init w in
  Alcotest.(check string) (at "code hash") (to_str (field "codeHash" spec))
    (hex (Cell.hash (Option.get si.Ton_tlb.Message.code)));
  Alcotest.(check string) (at "data hash") (to_str (field "dataHash" spec))
    (hex (Cell.hash (Option.get si.Ton_tlb.Message.data)));
  (* The address is the hash of the state init, so this checks both at once. *)
  Alcotest.(check string) (at "address") (to_str (field "address" spec))
    (Ton_address.to_raw (Wallet.address w));
  Alcotest.(check string) (at "friendly address") (to_str (field "addressFriendly" spec))
    (Ton_address.to_friendly (Wallet.address w));
  (* Signed transfer bodies, byte for byte. *)
  let ts = to_obj (field "transfers" spec) in
  List.iter
    (fun (tname, messages) ->
      let want = to_obj (field tname ts) in
      let valid_until = Int32.of_int (to_int (field "validUntil" want)) in
      let seqno = to_int (field "seqno" want) in
      let send_mode = to_int (field "sendMode" want) in
      let body =
        ok (name ^ "/" ^ tname)
          (Wallet.create_transfer w ~key:(Lazy.force key) ~seqno ~valid_until ~send_mode messages)
      in
      Alcotest.(check string)
        (Printf.sprintf "%s: %s body hash" name tname)
        (to_str (field "bodyHash" want)) (hex (Cell.hash body));
      Alcotest.(check string)
        (Printf.sprintf "%s: %s body bytes" name tname)
        (to_str (field "bodyBoc" want))
        (Base64.encode_string (Boc.serialize body));
      (* And the external message that carries it. *)
      let ext = Wallet.external_message w ~body in
      let ec = ok "external" (Ton_tlb.Message.to_cell ext) in
      Alcotest.(check string)
        (Printf.sprintf "%s: %s external hash" name tname)
        (to_str (field "externalHash" want)) (hex (Cell.hash ec)))
    transfers

let wallet_names = lazy (List.map fst (to_obj (section "wallets")))

(* --- relaxed messages ------------------------------------------------------ *)

(* Checked separately from the wallet layer so a mismatch points at the right
   place. *)
let test_relaxed () =
  let want = to_obj (section "relaxed") in
  List.iter
    (fun (tname, messages) ->
      let hashes = List.map to_str (to_list (field tname want)) in
      List.iteri
        (fun i m ->
          let c = ok "relaxed" (Ton_tlb.Message.to_cell m) in
          Alcotest.(check string) (Printf.sprintf "%s[%d]" tname i) (List.nth hashes i) (hex (Cell.hash c)))
        messages)
    transfers

(* --- behaviour ------------------------------------------------------------- *)

let pub_key = lazy (Ton_crypto.Ed25519.public (Lazy.force key))

let test_code_hashes () =
  (* These are published constants; if one moves, the embedded code is wrong. *)
  List.iter
    (fun (v, expected) -> Alcotest.(check string) (version_to_string v) expected (hex (Cell.hash (code v))))
    [ (V3R2, "84dafa449f98a6987789ba232358072bc0f76dc4524002a5d0918b9a75d2d599");
      (V4R2, "feb5ff6820e2ff0d9483e7e0d62c817d846789fb4ae580c878866d959dabd5c0");
      (V5R1, "20834b7b72b112147e1b2fb457b84e74d1a30f04f737d4f62a668e9552d2b72f") ]

let test_wallet_id_formula () =
  (* v3 and v4 offset a constant by the workchain. *)
  List.iter
    (fun wc ->
      let w = ok "v3" (create ~workchain:wc V3R2 ~public_key:(Lazy.force pub_key)) in
      Alcotest.(check int32)
        (Printf.sprintf "v3r2 wc=%d" wc)
        (Int32.add 698983191l (Int32.of_int wc))
        (Wallet.wallet_id w))
    [ 0; -1; 1; 2; -3 ];
  (* The four v5r1 constants every implementation quotes. *)
  List.iter
    (fun (network, wc, expected) ->
      let w = ok "v5" (create ~workchain:wc ~network V5R1 ~public_key:(Lazy.force pub_key)) in
      Alcotest.(check int32)
        (Printf.sprintf "v5r1 %s wc=%d" (match network with Mainnet -> "mainnet" | Testnet -> "testnet") wc)
        expected (Wallet.wallet_id w))
    [ (Mainnet, 0, 2147483409l); (Mainnet, -1, 8388369l);
      (Testnet, 0, 2147483645l); (Testnet, -1, 8388605l) ]

(* v3 and v4 put the signature before the payload, v5r1 after it. Both are
   512 bits, so length proves nothing -- recover the payload from the other
   end of the body and check the signature actually verifies over it. *)
let test_signature_placement () =
  let messages = [ internal ~dest ~value:(nano "1") () ] in
  let body_of v =
    let w = ok "w" (create v ~public_key:(Lazy.force pub_key)) in
    ok "body" (Wallet.create_transfer w ~key:(Lazy.force key) ~seqno:0 ~valid_until:1893456000l messages)
  in
  let split_at ~signature_first body =
    let bits = Cell.bits body in
    let n = Bits.length bits in
    let sign_bits, payload_bits =
      if signature_first then (Bits.sub bits 0 512, Bits.sub bits 512 (n - 512))
      else (Bits.sub bits (n - 512) 512, Bits.sub bits 0 (n - 512))
    in
    let payload =
      cell_of (List.fold_left Builder.store_ref (Builder.store_bits (Builder.create ()) payload_bits)
                 (Cell.refs body))
    in
    (Option.get (Bits.to_bytes sign_bits), payload)
  in
  let check v ~signature_first =
    let signature, payload = split_at ~signature_first (body_of v) in
    Alcotest.(check bool)
      (Printf.sprintf "%s signature verifies where expected" (version_to_string v))
      true
      (Ton_crypto.Ed25519.verify ~public:(Lazy.force pub_key) ~signature (Cell.hash payload));
    (* And it must not verify if we look at the wrong end. *)
    let wrong_signature, wrong_payload = split_at ~signature_first:(not signature_first) (body_of v) in
    Alcotest.(check bool)
      (Printf.sprintf "%s does not verify at the other end" (version_to_string v))
      false
      (Ton_crypto.Ed25519.verify ~public:(Lazy.force pub_key) ~signature:wrong_signature
         (Cell.hash wrong_payload))
  in
  check V3R2 ~signature_first:true;
  check V4R2 ~signature_first:true;
  check V5R1 ~signature_first:false

let test_too_many_messages () =
  let w = ok "w" (create V3R2 ~public_key:(Lazy.force pub_key)) in
  let messages = List.init 5 (fun _ -> internal ~dest ~value:(nano "1") ()) in
  match Wallet.create_transfer w ~key:(Lazy.force key) ~seqno:0 ~valid_until:0l messages with
  | Ok _ -> Alcotest.fail "accepted five messages"
  | Error e -> Alcotest.(check string) "error" "v3r2 accepts at most 4 messages, got 5" e;
  let w5 = ok "w5" (create V5R1 ~public_key:(Lazy.force pub_key)) in
  Alcotest.(check bool) "v5r1 accepts five" true
    (Result.is_ok (Wallet.create_transfer w5 ~key:(Lazy.force key) ~seqno:0 ~valid_until:0l messages))

let test_rejects_bad_key () =
  match create V3R2 ~public_key:"short" with
  | Ok _ -> Alcotest.fail "accepted a short public key"
  | Error e -> Alcotest.(check string) "error" "public key must be 32 bytes, got 5" e

let test_deploy_message_carries_init () =
  let w = ok "w" (create V4R2 ~public_key:(Lazy.force pub_key)) in
  let body =
    ok "body" (Wallet.create_transfer w ~key:(Lazy.force key) ~seqno:0 ~valid_until:1893456000l [])
  in
  let with_init = Wallet.external_message ~with_init:true w ~body in
  let without = Wallet.external_message w ~body in
  Alcotest.(check bool) "state init present" true (with_init.Ton_tlb.Message.init <> None);
  Alcotest.(check bool) "state init absent" true (without.Ton_tlb.Message.init = None);
  let a = ok "a" (Ton_tlb.Message.to_cell with_init) and b = ok "b" (Ton_tlb.Message.to_cell without) in
  Alcotest.(check bool) "different messages" false (String.equal (Cell.hash a) (Cell.hash b))

let () =
  Alcotest.run "wallet"
    [ ( "vs reference",
        List.map
          (fun n -> Alcotest.test_case n `Quick (wallet_case n (field n (to_obj (section "wallets")))))
          (Lazy.force wallet_names) );
      ("relaxed messages", [ Alcotest.test_case "hashes" `Quick test_relaxed ]);
      ( "behaviour",
        [ Alcotest.test_case "code hashes" `Quick test_code_hashes;
          Alcotest.test_case "wallet id formulas" `Quick test_wallet_id_formula;
          Alcotest.test_case "signature placement" `Quick test_signature_placement;
          Alcotest.test_case "message count limits" `Quick test_too_many_messages;
          Alcotest.test_case "bad public key" `Quick test_rejects_bad_key;
          Alcotest.test_case "deploy carries state init" `Quick test_deploy_message_carries_init ] )
    ]
