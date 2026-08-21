(* Fetches real account-state proofs and writes them as a test fixture, so
   proof verification can be developed and tested offline. Run by hand. *)
open Lwt.Infix
module L = Ton_lite_client_lwt
module C = Ton_lite_client

let b64 = Base64.encode_string
let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let () =
  Mirage_crypto_rng_unix.use_default ();
  let addresses =
    [ ("elector", "-1:3333333333333333333333333333333333333333333333333333333333333333");
      ("config", "-1:5555555555555555555555555555555555555555555555555555555555555555");
      (* A basechain address, so the answer carries a shard proof too. *)
      ("basechain_absent", "0:26d3866fcbb668acd911561f24f5d9ce4b59d22802c0be5cbebbfab5632e94aa") ]
  in
  let out = Buffer.create 4096 in
  let result =
    Lwt_main.run
      (L.with_connection ~host:"5.9.10.47" ~port:19949
         ~server_pub:(Result.get_ok (Base64.decode "n4VDnSCUuSpjnCyUk9e3QOOd6o0ItSWYbTnW3Wnn8wk="))
         (fun t ->
           L.call t C.Query.get_masterchain_info >>= function
           | Error e -> Lwt.return (Error e)
           | Ok info ->
               let b = info.C.Lite.last in
               Buffer.add_string out
                 (Printf.sprintf
                    "{\n \"note\": \"Real account-state answers, kept so proof verification can be tested offline.\",\n \"block\": { \"workchain\": %ld, \"shard\": \"%Lx\", \"seqno\": %ld, \"rootHash\": \"%s\", \"fileHash\": \"%s\" },\n \"accounts\": {"
                    b.C.Lite.workchain b.C.Lite.shard b.C.Lite.seqno (hex b.C.Lite.root_hash)
                    (hex b.C.Lite.file_hash));
               Lwt_list.fold_left_s
                 (fun acc (name, raw) ->
                   match acc with
                   | Error _ -> Lwt.return acc
                   | Ok first -> (
                       let addr = Result.get_ok (Ton_address.of_raw raw) in
                       L.call t (C.Query.get_account_state ~block:b addr) >>= function
                       | Error e -> Lwt.return (Error e)
                       | Ok st ->
                           Printf.eprintf "%-18s state=%6d proof=%6d shard_proof=%6d shardblk seqno=%ld\n%!"
                             name (String.length st.C.Lite.state) (String.length st.C.Lite.proof)
                             (String.length st.C.Lite.shard_proof) st.C.Lite.shardblk.C.Lite.seqno;
                           let sb = st.C.Lite.shardblk in
                           Buffer.add_string out
                             (Printf.sprintf
                                "%s\n  \"%s\": {\n   \"address\": \"%s\",\n   \"shardblk\": { \"workchain\": %ld, \"shard\": \"%Lx\", \"seqno\": %ld, \"rootHash\": \"%s\", \"fileHash\": \"%s\" },\n   \"state\": \"%s\",\n   \"proof\": \"%s\",\n   \"shardProof\": \"%s\"\n  }"
                                (if first then "" else ",")
                                name raw sb.C.Lite.workchain sb.C.Lite.shard sb.C.Lite.seqno
                                (hex sb.C.Lite.root_hash) (hex sb.C.Lite.file_hash)
                                (b64 st.C.Lite.state) (b64 st.C.Lite.proof) (b64 st.C.Lite.shard_proof));
                           Lwt.return (Ok false)))
                 (Ok true) addresses
               >>= function
               | Error e -> Lwt.return (Error e)
               | Ok _ ->
                   Buffer.add_string out "\n }\n}\n";
                   Lwt.return (Ok ())))
  in
  match result with
  | Ok () -> print_string (Buffer.contents out)
  | Error e -> Format.eprintf "FAILED: %a@." L.pp_error e; exit 1
