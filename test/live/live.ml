(* A live liteserver query. Not part of `dune runtest`: it needs the network.

   Usage: live.exe [HOST PORT BASE64_KEY]
   Defaults to the first mainnet liteserver in the published global config. *)
open Lwt.Infix
module L = Ton_lite_client_lwt
module C = Ton_lite_client

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let () =
  Mirage_crypto_rng_unix.use_default ();
  let host, port, key =
    if Array.length Sys.argv >= 4 then (Sys.argv.(1), int_of_string Sys.argv.(2), Sys.argv.(3))
    else ("5.9.10.47", 19949, "n4VDnSCUuSpjnCyUk9e3QOOd6o0ItSWYbTnW3Wnn8wk=")
  in
  let server_pub = Result.get_ok (Base64.decode key) in
  Printf.printf "connecting to %s:%d\n%!" host port;
  Printf.printf "  server key id %s\n%!" (hex (Ton_adnl.Key_id.of_ed25519_pub server_pub));
  let result =
    Lwt_main.run
      (L.with_connection ~host ~port ~server_pub (fun t ->
           Printf.printf "  handshake confirmed\n%!";
           L.call t C.Query.get_masterchain_info >>= function
           | Error e -> Lwt.return (Error e)
           | Ok info ->
               let last = info.C.Lite.last in
               Printf.printf "masterchain seqno   %ld\n" last.C.Lite.seqno;
               Printf.printf "  workchain         %ld\n" last.C.Lite.workchain;
               Printf.printf "  shard             %016Lx\n" last.C.Lite.shard;
               Printf.printf "  root hash         %s\n" (hex last.C.Lite.root_hash);
               Printf.printf "  file hash         %s\n%!" (hex last.C.Lite.file_hash);
               L.call t C.Query.get_time >>= function
               | Error e -> Lwt.return (Error e)
               | Ok time ->
                   Printf.printf "server time         %ld\n%!" time.C.Lite.now;
                   L.call t C.Query.get_version >>= function
                   | Error e -> Lwt.return (Error e)
                   | Ok v ->
                       Printf.printf "server version      %ld (capabilities %Ld)\n%!" v.C.Lite.version
                         v.C.Lite.capabilities;
                       L.ping t >>= fun p ->
                       Printf.printf "ping                %s\n%!"
                         (match p with Ok id -> Printf.sprintf "pong %Ld" id | Error _ -> "failed");
                       (* The elector, which every mainnet node has. Fetching
                          and parsing it exercises the whole stack: ADNL, TL,
                          the Bag of Cells and the TL-B account schema. *)
                       let addr =
                         Result.get_ok
                           (Ton_address.of_raw
                              "-1:3333333333333333333333333333333333333333333333333333333333333333")
                       in
                       L.call t (C.Query.get_account_state ~block:last addr) >>= function
                       | Error e -> Lwt.return (Error e)
                       | Ok st ->
                           Printf.printf "account %s\n" (Ton_address.to_raw addr);
                           Printf.printf "  state boc         %d bytes\n" (String.length st.C.Lite.state);
                           Printf.printf "  proof             %d bytes\n" (String.length st.C.Lite.proof);
                           Printf.printf "  shard proof       %d bytes\n%!" (String.length st.C.Lite.shard_proof);
                           (match Ton_cell.Boc.deserialize_root st.C.Lite.state with
                           | Error e -> Format.printf "  BOC ERROR %a@." Ton_cell.Boc.pp_error e
                           | Ok cell -> (
                               match Ton_tlb.Account.of_cell cell with
                               | Error e -> Format.printf "  ACCOUNT ERROR %a@." Ton_cell.Slice.pp_error e
                               | Ok None -> print_endline "  account does not exist"
                               | Ok (Some a) ->
                                   Printf.printf "  balance           %s TON\n"
                                     (Ton_tlb.Coins.to_string (Ton_tlb.Account.balance a));
                                   Printf.printf "  state             %s\n"
                                     (match a.Ton_tlb.Account.storage.state with
                                     | Ton_tlb.Account.Uninit -> "uninit"
                                     | Ton_tlb.Account.Active _ -> "active"
                                     | Ton_tlb.Account.Frozen _ -> "frozen");
                                   Printf.printf "  code hash         %s\n%!"
                                     (match Ton_tlb.Account.code a with
                                     | Some c -> hex (Ton_cell.Cell.hash c)
                                     | None -> "-")));
                           Lwt.return (Ok ())))
  in
  match result with
  | Ok () -> print_endline "OK"
  | Error e -> Format.printf "FAILED: %a@." L.pp_error e; exit 1
