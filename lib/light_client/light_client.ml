(* The unikernel's actual work, kept out of the MirageOS entry point so it
   compiles and type-checks in the ordinary build rather than only under
   `mirage configure`.

   There is very little here, which is the point: everything that parses a
   cell, checks a proof or drives ADNL is the same pure code the offline test
   suite runs. *)

open Lwt.Infix
module C = Ton_lite_client

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

module Make (S : Tcpip.Stack.V4V6) = struct
  module Flow = Ton_lite_client_mirage.Make (S.TCP)

  let log fmt = Printf.ksprintf (fun s -> Logs.info (fun m -> m "%s" s)) fmt

  let report ~mc_root ~addr (st : C.Lite.lite_server_account_state) =
    let sb = st.C.Lite.shardblk in
    log "account %s" (Ton_address.to_raw addr);
    log "  state %d bytes, proof %d, shard proof %d" (String.length st.C.Lite.state)
      (String.length st.C.Lite.proof)
      (String.length st.C.Lite.shard_proof);
    let shardblk =
      { Ton_proof.Account.workchain = sb.C.Lite.workchain; shard = sb.C.Lite.shard;
        seqno = sb.C.Lite.seqno; root_hash = sb.C.Lite.root_hash }
    in
    (* Verify rather than believe: the whole chain from the masterchain block,
       through the shard link, to the account's committed hash. *)
    match
      Ton_proof.Account.verify_via_shard ~mc_root_hash:mc_root ~shard_proof:st.C.Lite.shard_proof
        ~shardblk ~proof:st.C.Lite.proof ~state:st.C.Lite.state ~address:addr
    with
    | Error e -> log "  PROOF FAILED: %s" (Format.asprintf "%a" Ton_proof.Account.pp_error e)
    | Ok Ton_proof.Account.Does_not_exist -> log "  verified: the account does not exist"
    | Ok (Ton_proof.Account.Exists cell) -> (
        log "  verified against masterchain block %s" (String.sub (hex mc_root) 0 16);
        match Ton_tlb.Account.of_cell cell with
        | Ok (Some a) ->
            log "  balance %s TON" (Ton_tlb.Coins.to_string (Ton_tlb.Account.balance a));
            log "  code hash %s"
              (match Ton_tlb.Account.code a with Some c -> hex (Ton_cell.Cell.hash c) | None -> "-")
        | Ok None -> log "  account_none"
        | Error e -> log "  could not parse the account: %s" (Format.asprintf "%a" Ton_cell.Slice.pp_error e))

  let run stack ~host ~port ~server_pub ~account =
    log "connecting to %s:%d" (Ipaddr.to_string host) port;
    S.TCP.create_connection (S.tcp stack) (host, port) >>= function
    | Error e -> log "could not connect: %s" (Format.asprintf "%a" S.TCP.pp_error e); Lwt.return_unit
    | Ok flow -> (
        Flow.connect ~random:Mirage_crypto_rng.generate flow ~server_pub >>= function
        | Error e ->
            log "handshake failed: %s" (Format.asprintf "%a" Ton_lite_client_mirage.pp_error e);
            Lwt.return_unit
        | Ok t -> (
            log "handshake confirmed";
            Flow.call t C.Query.get_masterchain_info >>= function
            | Error e ->
                log "query failed: %s" (Format.asprintf "%a" Ton_lite_client_mirage.pp_error e);
                Flow.close t
            | Ok info -> (
                let last = info.C.Lite.last in
                log "masterchain seqno %ld" last.C.Lite.seqno;
                log "  root hash %s" (hex last.C.Lite.root_hash);
                match Ton_address.of_raw account with
                | Error e ->
                    log "bad account address: %s" (Format.asprintf "%a" Ton_address.pp_error e);
                    Flow.close t
                | Ok addr -> (
                    Flow.call t (C.Query.get_account_state ~block:last addr) >>= function
                    | Error e ->
                        log "account query failed: %s"
                          (Format.asprintf "%a" Ton_lite_client_mirage.pp_error e);
                        Flow.close t
                    | Ok st ->
                        report ~mc_root:last.C.Lite.root_hash ~addr st;
                        Flow.close t))))
end
