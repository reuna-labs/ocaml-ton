open Ton_cell
module M = Ton_tlb.Message
module A = Ton_tlb.Msg_address

type version = V3R2 | V4R2 | V5R1

let version_to_string = function V3R2 -> "v3r2" | V4R2 -> "v4r2" | V5R1 -> "v5r1"

let code_boc = function
  | V3R2 -> Wallet_code.v3r2
  | V4R2 -> Wallet_code.v4r2
  | V5R1 -> Wallet_code.v5r1

let decode_code v =
  match Base64.decode (code_boc v) with
  | Error (`Msg m) -> failwith ("wallet code is not valid base64: " ^ m)
  | Ok raw -> (
      match Boc.deserialize_root raw with
      | Ok c -> c
      | Error e -> failwith (Format.asprintf "wallet code: %a" Boc.pp_error e))

let codes = lazy (List.map (fun v -> (v, decode_code v)) [ V3R2; V4R2; V5R1 ])
let code v = List.assoc v (Lazy.force codes)

type network = Mainnet | Testnet

let global_id = function Mainnet -> -239l | Testnet -> -3l
let max_messages = function V3R2 | V4R2 -> 4 | V5R1 -> 255

(* v3 and v4 simply offset a fixed constant by the workchain. *)
let default_subwallet_id = 698983191l

(* v5r1 packs the workchain, a version byte and a subwallet counter into a
   32-bit context and exclusive-ors it with the network's global id, so the
   workchain is part of the wallet's identity rather than just its address.

     context_id_client$1 wc:int8 wallet_version:uint8 counter:uint15 *)
let v5r1_wallet_id ~network ~workchain ~subwallet =
  let ctx =
    Int32.logor 0x80000000l
      (Int32.logor
         (Int32.shift_left (Int32.of_int (workchain land 0xff)) 23)
         (Int32.of_int (subwallet land 0x7fff)))
  in
  Int32.logxor (global_id network) ctx

type t = {
  version : version;
  workchain : int;
  public_key : string;
  wallet_id : int32;
  state_init : M.state_init;
  address : Ton_address.t;
}

let ( let* ) = Result.bind
let builder_err e = Format.asprintf "%a" Builder.pp_error e
let cell_of b = Result.map_error builder_err (Builder.end_cell b)

let data_cell version ~wallet_id ~public_key =
  let b = Builder.create () in
  let b =
    match version with
    | V3R2 | V4R2 ->
        let b = Builder.store_uint b 0L ~bits:32 (* seqno *) in
        let b = Builder.store_uint b (Int64.of_int32 wallet_id) ~bits:32 in
        Builder.store_bytes b public_key
    | V5R1 ->
        let b = Builder.store_bit b true (* is_signature_allowed *) in
        let b = Builder.store_uint b 0L ~bits:32 (* seqno *) in
        let b = Builder.store_uint b (Int64.of_int32 wallet_id) ~bits:32 in
        Builder.store_bytes b public_key
  in
  (* v4 keeps a plugin dictionary and v5 an extension dictionary; both start
     empty, which is a single zero bit. v3 has neither. *)
  let b = match version with V3R2 -> b | V4R2 | V5R1 -> Builder.store_bit b false in
  cell_of b

let create ?(workchain = 0) ?(network = Mainnet) ?(subwallet = 0) ?wallet_id version ~public_key =
  if String.length public_key <> 32 then
    Error (Printf.sprintf "public key must be 32 bytes, got %d" (String.length public_key))
  else
    let wallet_id =
      match wallet_id with
      | Some id -> id
      | None -> (
          match version with
          | V3R2 | V4R2 -> Int32.add default_subwallet_id (Int32.of_int workchain)
          | V5R1 -> v5r1_wallet_id ~network ~workchain ~subwallet)
    in
    let* data = data_cell version ~wallet_id ~public_key in
    let state_init = { M.empty_state_init with code = Some (code version); data = Some data } in
    let* address = M.state_init_address ~workchain state_init in
    Ok { version; workchain; public_key; wallet_id; state_init; address }

let version t = t.version
let workchain t = t.workchain
let public_key t = t.public_key
let wallet_id t = t.wallet_id
let state_init t = t.state_init
let address t = t.address

(* --- outgoing messages ----------------------------------------------------- *)

let internal ?(bounce = true) ?body ?init ~dest ~value () =
  { M.info =
      M.Internal
        { ihr_disabled = true; bounce; bounced = false;
          (* Relaxed: the validator fills in the source. *)
          src = A.Addr_none;
          dest = A.of_address dest;
          value = Ton_tlb.Currency.of_coins value;
          ihr_fee = Z.zero; fwd_fee = Z.zero; created_lt = 0L; created_at = 0l };
    init;
    body = (match body with Some c -> c | None -> Cell.empty) }

let send_mode_default = 3

let message_cell m = Result.map_error (fun e -> e) (M.to_cell m)

(* v3 and v4: wallet_id, deadline, seqno, then (mode, ^message) pairs. v4
   additionally carries an 8-bit opcode, which the documentation wrongly
   describes as 32-bit. *)
let sign_payload_v3_v4 version ~wallet_id ~valid_until ~seqno ~send_mode messages =
  let b = Builder.create () in
  let b = Builder.store_uint b (Int64.of_int32 wallet_id) ~bits:32 in
  let b = Builder.store_uint b (Int64.of_int32 valid_until) ~bits:32 in
  let b = Builder.store_uint b (Int64.of_int seqno) ~bits:32 in
  let b = match version with V4R2 -> Builder.store_uint b 0L ~bits:8 | _ -> b in
  List.fold_left
    (fun acc m ->
      let* acc = acc in
      let* c = message_cell m in
      Ok (Builder.store_ref (Builder.store_uint acc (Int64.of_int send_mode) ~bits:8) c))
    (Ok b) messages

(* v5r1 out actions are a cons list threaded through references, newest
   outermost:  out_list$_ prev:^(OutList n) action:OutAction = OutList (n+1) *)
let action_send_msg = 0x0ec3c86dL

let out_list ~send_mode messages =
  List.fold_left
    (fun acc m ->
      let* prev = acc in
      let* c = message_cell m in
      let b = Builder.store_ref (Builder.create ()) prev in
      let b = Builder.store_uint b action_send_msg ~bits:32 in
      let b = Builder.store_uint b (Int64.of_int send_mode) ~bits:8 in
      cell_of (Builder.store_ref b c))
    (cell_of (Builder.create ()))
    messages

let w5_external_signed = 0x7369676eL (* "sign" *)

let sign_payload_v5 ~wallet_id ~valid_until ~seqno ~send_mode messages =
  let b = Builder.create () in
  let b = Builder.store_uint b w5_external_signed ~bits:32 in
  let b = Builder.store_uint b (Int64.of_int32 wallet_id) ~bits:32 in
  let b = Builder.store_uint b (Int64.of_int32 valid_until) ~bits:32 in
  let b = Builder.store_uint b (Int64.of_int seqno) ~bits:32 in
  if messages = [] then
    (* No out actions, no extended actions. *)
    Ok (Builder.store_bit (Builder.store_bit b false) false)
  else
    let* actions = out_list ~send_mode messages in
    let b = Builder.store_ref (Builder.store_bit b true) actions in
    Ok (Builder.store_bit b false)

let create_transfer t ~key ~seqno ~valid_until ?(send_mode = send_mode_default) messages =
  let n = List.length messages in
  if n > max_messages t.version then
    Error
      (Printf.sprintf "%s accepts at most %d messages, got %d" (version_to_string t.version)
         (max_messages t.version) n)
  else
    let* payload =
      match t.version with
      | V3R2 | V4R2 ->
          sign_payload_v3_v4 t.version ~wallet_id:t.wallet_id ~valid_until ~seqno ~send_mode messages
      | V5R1 -> sign_payload_v5 ~wallet_id:t.wallet_id ~valid_until ~seqno ~send_mode messages
    in
    let* payload_cell = cell_of payload in
    let signature = Ton_crypto.Ed25519.sign key (Cell.hash payload_cell) in
    match t.version with
    | V3R2 | V4R2 ->
        (* Signature first, then the payload spliced in. *)
        let b = Builder.store_bytes (Builder.create ()) signature in
        let b = Builder.store_bits b (Cell.bits payload_cell) in
        cell_of (List.fold_left Builder.store_ref b (Cell.refs payload_cell))
    | V5R1 ->
        (* Signature last. *)
        let b = Builder.store_bits (Builder.create ()) (Cell.bits payload_cell) in
        let b = List.fold_left Builder.store_ref b (Cell.refs payload_cell) in
        cell_of (Builder.store_bytes b signature)

let external_message ?(with_init = false) t ~body =
  { M.info =
      M.External_in { src = A.Addr_none; dest = A.of_address t.address; import_fee = Z.zero };
    init = (if with_init then Some t.state_init else None);
    body }
