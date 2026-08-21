(* A live liteserver query. Not part of `dune runtest`: it needs the network.

   Usage: live.exe [HOST PORT BASE64_KEY]
   Defaults to the first mainnet liteserver in the published global config. *)
open Lwt.Infix
module L = Ton_lite_client_lwt
module C = Ton_lite_client

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let ( let** ) x f = x >>= function Error e -> Lwt.return (Error e) | Ok v -> f v

(* The elector is a masterchain account every node has; the second is an
   ordinary basechain address, which exercises the shard link. *)
let addresses =
  [ "-1:3333333333333333333333333333333333333333333333333333333333333333";
    "0:26d3866fcbb668acd911561f24f5d9ce4b59d22802c0be5cbebbfab5632e94aa" ]

let show_account ~mc_root ~addr (st : C.Lite.lite_server_account_state) =
  let sb = st.C.Lite.shardblk in
  Printf.printf "account %s\n" (Ton_address.to_raw addr);
  Printf.printf "  shard block        %ld:%016Lx seqno %ld\n" sb.C.Lite.workchain sb.C.Lite.shard
    sb.C.Lite.seqno;
  Printf.printf "  state              %d bytes\n" (String.length st.C.Lite.state);
  Printf.printf "  proof              %d bytes (shard proof %d)\n%!" (String.length st.C.Lite.proof)
    (String.length st.C.Lite.shard_proof);
  let shardblk =
    { Ton_proof.Account.workchain = sb.C.Lite.workchain; shard = sb.C.Lite.shard;
      seqno = sb.C.Lite.seqno; root_hash = sb.C.Lite.root_hash }
  in
  (* The whole chain from the masterchain block: the shard link first, then
     the account. Verifying only the account would leave the server free to
     choose which shard block it was proved against. *)
  match
    Ton_proof.Account.verify_via_shard ~mc_root_hash:mc_root ~shard_proof:st.C.Lite.shard_proof
      ~shardblk ~proof:st.C.Lite.proof ~state:st.C.Lite.state ~address:addr
  with
  | Error e -> Format.printf "  PROOF FAILED       %a@." Ton_proof.Account.pp_error e
  | Ok Ton_proof.Account.Does_not_exist ->
      Printf.printf "  verified           the account does not exist\n%!"
  | Ok (Ton_proof.Account.Exists cell) -> (
      Printf.printf "  verified           against masterchain block %s…\n" (String.sub (hex mc_root) 0 16);
      match Ton_tlb.Account.of_cell cell with
      | Error e -> Format.printf "  ACCOUNT ERROR      %a@." Ton_cell.Slice.pp_error e
      | Ok None -> print_endline "  account_none"
      | Ok (Some a) ->
          Printf.printf "  balance            %s TON\n" (Ton_tlb.Coins.to_string (Ton_tlb.Account.balance a));
          Printf.printf "  state              %s\n"
            (match a.Ton_tlb.Account.storage.state with
            | Ton_tlb.Account.Uninit -> "uninit"
            | Ton_tlb.Account.Active _ -> "active"
            | Ton_tlb.Account.Frozen _ -> "frozen");
          Printf.printf "  code hash          %s\n%!"
            (match Ton_tlb.Account.code a with Some c -> hex (Ton_cell.Cell.hash c) | None -> "-"))

let () =
  Mirage_crypto_rng_unix.use_default ();
  let host, port, key =
    if Array.length Sys.argv >= 4 then (Sys.argv.(1), int_of_string Sys.argv.(2), Sys.argv.(3))
    else ("5.9.10.47", 19949, "n4VDnSCUuSpjnCyUk9e3QOOd6o0ItSWYbTnW3Wnn8wk=")
  in
  let server_pub = Result.get_ok (Base64.decode key) in
  Printf.printf "connecting to %s:%d\n" host port;
  Printf.printf "  server key id     %s\n%!" (hex (Ton_adnl.Key_id.of_ed25519_pub server_pub));
  let result =
    Lwt_main.run
      (L.with_connection ~host ~port ~server_pub (fun t ->
           Printf.printf "  handshake         confirmed\n%!";
           let** info = L.call t C.Query.get_masterchain_info in
           let last = info.C.Lite.last in
           Printf.printf "masterchain seqno  %ld\n" last.C.Lite.seqno;
           Printf.printf "  root hash          %s\n%!" (hex last.C.Lite.root_hash);
           let** time = L.call t C.Query.get_time in
           Printf.printf "server time        %ld\n%!" time.C.Lite.now;
           let** v = L.call t C.Query.get_version in
           Printf.printf "server version     %ld (capabilities %Ld)\n%!" v.C.Lite.version
             v.C.Lite.capabilities;
           L.ping t >>= fun p ->
           Printf.printf "ping               %s\n%!"
             (match p with Ok id -> Printf.sprintf "pong %Ld" id | Error _ -> "failed");
           Lwt_list.fold_left_s
             (fun acc raw ->
               match acc with
               | Error _ -> Lwt.return acc
               | Ok () ->
                   let addr = Result.get_ok (Ton_address.of_raw raw) in
                   let** st = L.call t (C.Query.get_account_state ~block:last addr) in
                   show_account ~mc_root:last.C.Lite.root_hash ~addr st;
                   Lwt.return (Ok ()))
             (Ok ()) addresses))
  in
  match result with
  | Ok () -> print_endline "OK"
  | Error e ->
      Format.printf "FAILED: %a@." L.pp_error e;
      exit 1
