(* Fetches a real block-proof chain and writes it as a fixture. Run by hand. *)
open Lwt.Infix
module L = Ton_lite_client_lwt
module C = Ton_lite_client

let b64 = Base64.encode_string
let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))
let d64 s = Result.get_ok (Base64.decode s)

(* The trust anchor published in the global config. *)
let init_block : C.Lite.ton_node_block_id_ext =
  { workchain = -1l; shard = 0x8000000000000000L; seqno = 46894135l;
    root_hash = d64 "MEjmmhLPlG68mbTPnKYcP/Sz/MiMQBV2OsASBOzBv58=";
    file_hash = d64 "u9rAtFQ+kUFEnOs3w8Y7punMTiyQTXf1bRfkSs8dG+0=" }

let blk_json (b : C.Lite.ton_node_block_id_ext) =
  Printf.sprintf
    "{ \"workchain\": %ld, \"shard\": \"%Lx\", \"seqno\": %ld, \"rootHash\": \"%s\", \"fileHash\": \"%s\" }"
    b.workchain b.shard b.seqno (hex b.root_hash) (hex b.file_hash)

let ( let** ) x f = x >>= function Error e -> Lwt.return (Error e) | Ok v -> f v

let sigs_json = function
  | C.Lite.Ordinary (o : C.Lite.lite_server_signature_set_ordinary) ->
      Printf.sprintf
        "{ \"kind\": \"ordinary\", \"validatorSetHash\": %ld, \"catchainSeqno\": %ld, \"signatures\": [%s] }"
        o.validator_set_hash o.catchain_seqno
        (String.concat ", "
           (List.map
              (fun (g : C.Lite.lite_server_signature) ->
                Printf.sprintf "{ \"who\": \"%s\", \"signature\": \"%s\" }" (hex g.node_id_short)
                  (b64 g.signature))
              o.signatures))
  | C.Lite.Simplex _ -> "{ \"kind\": \"simplex\" }"

let step_json sep = function
  | C.Lite.Back (b : C.Lite.lite_server_block_link_back) ->
      Printf.sprintf
        "%s{ \"kind\": \"back\", \"toKeyBlock\": %b, \"from\": %s, \"to\": %s, \"destProof\": \"%s\", \"proof\": \"%s\", \"stateProof\": \"%s\" }"
        sep b.to_key_block (blk_json b.from) (blk_json b.to_) (b64 b.dest_proof) (b64 b.proof)
        (b64 b.state_proof)
  | C.Lite.Forward (f : C.Lite.lite_server_block_link_forward) ->
      Printf.sprintf
        "%s{ \"kind\": \"forward\", \"toKeyBlock\": %b, \"from\": %s, \"to\": %s, \"destProof\": \"%s\", \"configProof\": \"%s\", \"signatures\": %s }"
        sep f.to_key_block (blk_json f.from) (blk_json f.to_) (b64 f.dest_proof) (b64 f.config_proof)
        (sigs_json f.signatures)

let describe i = function
  | C.Lite.Back (b : C.Lite.lite_server_block_link_back) ->
      Printf.eprintf "  [%d] back    to_key=%-5b %ld -> %ld  dest=%d proof=%d state=%d\n%!" i
        b.to_key_block b.from.seqno b.to_.seqno (String.length b.dest_proof) (String.length b.proof)
        (String.length b.state_proof)
  | C.Lite.Forward (f : C.Lite.lite_server_block_link_forward) ->
      Printf.eprintf "  [%d] forward to_key=%-5b %ld -> %ld  dest=%d config=%d sigs=%d\n%!" i
        f.to_key_block f.from.seqno f.to_.seqno (String.length f.dest_proof)
        (String.length f.config_proof)
        (match f.signatures with
        | C.Lite.Ordinary o -> List.length o.signatures
        | C.Lite.Simplex s -> List.length s.signatures)

let () =
  Mirage_crypto_rng_unix.use_default ();
  let out = Buffer.create 65536 in
  let result =
    Lwt_main.run
      (L.with_connection ~host:"5.9.10.47" ~port:19949
         ~server_pub:(d64 "n4VDnSCUuSpjnCyUk9e3QOOd6o0ItSWYbTnW3Wnn8wk=")
         (fun t ->
           let** info = L.call t C.Query.get_masterchain_info in
           let head = info.C.Lite.last in
           Printf.eprintf "head seqno %ld\n%!" head.seqno;
           let** p = L.call t (C.Query.get_block_proof ~known:init_block ~target:head ()) in
           Printf.eprintf "complete=%b steps=%d from=%ld to=%ld\n%!" p.C.Lite.complete
             (List.length p.C.Lite.steps) p.C.Lite.from.seqno p.C.Lite.to_.seqno;
           List.iteri describe p.C.Lite.steps;
           Buffer.add_string out
             (Printf.sprintf
                "{\n \"note\": \"A real block-proof chain from the global config's init block.\",\n \"initBlock\": %s,\n \"head\": %s,\n \"complete\": %b,\n \"from\": %s,\n \"to\": %s,\n \"steps\": [\n"
                (blk_json init_block) (blk_json head) p.C.Lite.complete (blk_json p.C.Lite.from)
                (blk_json p.C.Lite.to_));
           List.iteri (fun i s -> Buffer.add_string out (step_json (if i = 0 then "  " else ",\n  ") s))
             p.C.Lite.steps;
           Buffer.add_string out "\n ]\n}\n";
           Lwt.return (Ok ())))
  in
  match result with
  | Ok () -> print_string (Buffer.contents out)
  | Error e ->
      Format.eprintf "FAILED: %a@." L.pp_error e;
      exit 1
