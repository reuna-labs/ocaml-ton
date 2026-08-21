(* Derives a trusted masterchain block by walking key-block links from the
   anchor published in TON's global configuration, verifying validator
   signatures at every step. Needs the network; not part of `dune runtest`.

   Bootstrapping all the way from the configured init block is expensive: key
   blocks come every few thousand blocks, a liteserver returns a bounded
   number of links per call, and each carries a validator set. A deployment
   persists the last block it verified and starts from there; this walks a
   bounded number of rounds to show the mechanism, and says where it got to. *)
open Lwt.Infix
module L = Ton_lite_client_lwt
module C = Ton_lite_client
module BP = Ton_proof.Block_proof

let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))
let d64 s = Result.get_ok (Base64.decode s)
let ( let** ) x f = x >>= function Error e -> Lwt.return (Error e) | Ok v -> f v

let anchor : C.Lite.ton_node_block_id_ext =
  { workchain = -1l; shard = 0x8000000000000000L; seqno = 46894135l;
    root_hash = d64 "MEjmmhLPlG68mbTPnKYcP/Sz/MiMQBV2OsASBOzBv58=";
    file_hash = d64 "u9rAtFQ+kUFEnOs3w8Y7punMTiyQTXf1bRfkSs8dG+0=" }

let block_of (b : C.Lite.ton_node_block_id_ext) =
  { BP.seqno = b.seqno; root_hash = b.root_hash; file_hash = b.file_hash }

let signatures = function
  | C.Lite.Ordinary (o : C.Lite.lite_server_signature_set_ordinary) ->
      List.map
        (fun (g : C.Lite.lite_server_signature) -> { BP.who = g.node_id_short; signature = g.signature })
        o.signatures
  | C.Lite.Simplex _ -> []

let link_of = function
  | C.Lite.Forward (f : C.Lite.lite_server_block_link_forward) ->
      BP.Forward
        { source = block_of f.from; dest = block_of f.to_; config_proof = f.config_proof;
          dest_proof = f.dest_proof; signatures = signatures f.signatures }
  | C.Lite.Back (b : C.Lite.lite_server_block_link_back) ->
      BP.Backward { source = block_of b.from; dest = block_of b.to_ }

(* Each round is one liteserver call returning a bounded number of links.
   Raise it to bootstrap further; a full walk from the configured anchor is
   hundreds of rounds and hundreds of megabytes, which is the argument for
   persisting what has been verified. *)
let max_rounds = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 3

let () =
  Mirage_crypto_rng_unix.use_default ();
  let result =
    Lwt_main.run
      (L.with_connection ~host:"5.9.10.47" ~port:19949
         ~server_pub:(d64 "n4VDnSCUuSpjnCyUk9e3QOOd6o0ItSWYbTnW3Wnn8wk=")
         (fun t ->
           let** info = L.call t C.Query.get_masterchain_info in
           let head = info.C.Lite.last in
           Printf.printf "the server says the head is seqno %ld\n" head.C.Lite.seqno;
           Printf.printf "anchor (from the global config) is seqno %ld\n%!" anchor.seqno;
           let rec walk trusted known round =
             if round > max_rounds then Lwt.return (Ok (trusted, false))
             else
               let** p = L.call t (C.Query.get_block_proof ~known ~target:head ()) in
               let links = List.map link_of p.C.Lite.steps in
               match BP.follow_all trusted links with
               | Error e ->
                   Format.printf "  round %d: VERIFICATION FAILED %a@." round BP.pp_error e;
                   Lwt.return (Ok (trusted, false))
               | Ok next ->
                   Printf.printf "  round %d: %d links verified, trust now at seqno %ld%s\n%!" round
                     (List.length links) next.BP.seqno
                     (if p.C.Lite.complete then " (complete)" else "");
                   if p.C.Lite.complete then Lwt.return (Ok (next, true))
                   else
                     let known' =
                       { known with C.Lite.seqno = next.BP.seqno; root_hash = next.BP.root_hash;
                         file_hash = next.BP.file_hash }
                     in
                     walk next known' (round + 1)
           in
           let** trusted, complete = walk (block_of anchor) anchor 1 in
           Printf.printf "\nderived a trusted block at seqno %ld\n" trusted.BP.seqno;
           Printf.printf "  root hash %s\n" (hex trusted.BP.root_hash);
           if complete then
             Printf.printf "  which is the head the server named: %b\n%!"
               (String.equal trusted.BP.root_hash head.C.Lite.root_hash)
           else
             Printf.printf
               "  stopped after %d rounds; %ld blocks short of the head, which is why a\n\
               \  deployment persists what it has verified rather than starting over\n%!"
               max_rounds
               (Int32.sub head.C.Lite.seqno trusted.BP.seqno);
           Lwt.return (Ok ())))
  in
  match result with
  | Ok () -> print_endline "OK"
  | Error e -> Format.printf "FAILED: %a@." L.pp_error e; exit 1
