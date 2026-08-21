open Ton_cell

type block = { seqno : int32; root_hash : string; file_hash : string }
type signature = { who : string; signature : string }

type outcome = {
  next : block;
  signed_weight : int64;
  total_weight : int64;
  accepted : int;
  offered : int;
}

type error =
  | Boc of Boc.error
  | Merkle of Merkle.error
  | State of State.error
  | Config of Config.error
  | Validators of Validators.error
  | No_proof_of of string
  | No_validator_set
  | Insufficient_weight of { signed : int64; total : int64 }
  | Dest_mismatch of { want : string; got : string }
  | Link_does_not_continue of { trusted : string; starts_at : string }
  | Backward_link

let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let pp_error ppf = function
  | Boc e -> Boc.pp_error ppf e
  | Merkle e -> Merkle.pp_error ppf e
  | State e -> State.pp_error ppf e
  | Config e -> Config.pp_error ppf e
  | Validators e -> Validators.pp_error ppf e
  | No_proof_of what -> Format.fprintf ppf "no proof of %s among the roots" what
  | No_validator_set -> Format.fprintf ppf "the source block's configuration names no validator set"
  | Insufficient_weight { signed; total } ->
      Format.fprintf ppf "signatures carry %Ld of %Ld weight, short of the two-thirds threshold" signed
        total
  | Dest_mismatch { want; got } ->
      Format.fprintf ppf "the destination proof is for block %s, not %s" got want
  | Link_does_not_continue { trusted; starts_at } ->
      Format.fprintf ppf "the link starts at %s but trust reaches only %s" starts_at trusted
  | Backward_link ->
      Format.fprintf ppf "backward links are not followed: proving an older block from a newer one is not implemented"

let ( let* ) = Result.bind

let to_sign b =
  Ton_tl.Tl.Writer.to_string (fun w ->
      Ton_tl_schema.Adnl.write_boxed_ton_block_id w
        { Ton_tl_schema.Adnl.root_cell_hash = b.root_hash; file_hash = b.file_hash })

let find_proof_for roots hash =
  List.find_map
    (fun c -> match Merkle.proof c with Ok p when String.equal p.Merkle.hash hash -> Some p | _ -> None)
    roots

(* The configuration proof is a single Merkle proof of the key block itself.
   No state proof is involved: a key block carries the configuration in its
   own extra, which is exactly what makes it usable as a link in the chain. *)
let validator_set_of_config_proof ~from_root_hash ~config_proof =
  let* roots = Result.map_error (fun e -> Boc e) (Boc.deserialize config_proof) in
  let* block_proof =
    match find_proof_for roots from_root_hash with
    | Some p -> Ok p
    | None -> Error (No_proof_of (Printf.sprintf "block %s" (hex from_root_hash)))
  in
  let* config =
    Result.map_error (fun e -> Config e) (Config.of_key_block block_proof.Merkle.root)
  in
  (* A proof carries only the parameters it needs, so one of the two sets is
     usually pruned away. Not being shown a set means it cannot be used, not
     that the proof is broken -- and it costs nothing, because a link is only
     accepted when a set that *was* shown reaches the threshold. *)
  let one n =
    match Config.param config n with
    | Error (Config.Elided _) -> Ok None
    | Error e -> Error (Config e)
    | Ok None -> Ok None
    | Ok (Some c) -> (
        match Validators.of_cell c with
        | Ok v -> Ok (Some v)
        | Error (Validators.Elided _) -> Ok None
        | Error e -> Error (Validators e))
  in
  let* current = one Config.current_validators in
  let* next = one Config.next_validators in
  match List.filter_map Fun.id [ current; next ] with
  | [] -> Error No_validator_set
  | sets -> Ok sets

(* More than two thirds of the total weight, which is the threshold TON's
   consensus is built on. Compared by multiplication rather than division so
   the boundary is exact. *)
let meets_threshold ~signed ~total = Int64.compare (Int64.mul signed 3L) (Int64.mul total 2L) > 0

let tally set ~message ~signatures =
  List.fold_left
    (fun (weight, accepted) s ->
      match Validators.find set ~short_id:s.who with
      | None -> (weight, accepted)
      | Some v ->
          if Ton_crypto.Ed25519.verify ~public:v.Validators.public_key ~signature:s.signature message
          then (Int64.add weight v.Validators.weight, accepted + 1)
          else (weight, accepted))
    (0L, 0) signatures

let verify_forward ~from_root_hash ~config_proof ~dest_proof ~dest ~signatures =
  (* The destination proof must be about the block we are being told to trust. *)
  let* dest_roots = Result.map_error (fun e -> Boc e) (Boc.deserialize dest_proof) in
  let* () =
    match find_proof_for dest_roots dest.root_hash with
    | Some _ -> Ok ()
    | None ->
        let got =
          match dest_roots with
          | c :: _ -> ( match Merkle.proof c with Ok p -> hex p.Merkle.hash | Error _ -> "?")
          | [] -> "nothing"
        in
        Error (Dest_mismatch { want = hex dest.root_hash; got })
  in
  let* sets = validator_set_of_config_proof ~from_root_hash ~config_proof in
  let message = to_sign dest in
  (* Either set the source block appoints is a legitimate authority, so the
     link stands if any of them reaches the threshold. Using the wrong one
     simply fails to gather weight -- the signatures would not match its
     members. *)
  let best =
    List.fold_left
      (fun best set ->
        let signed, accepted = tally set ~message ~signatures in
        let candidate = (signed, Validators.main_weight set, accepted) in
        match best with
        | Some (bs, _, _) when Int64.compare bs signed >= 0 -> best
        | _ -> Some candidate)
      None sets
  in
  match best with
  | None -> Error No_validator_set
  | Some (signed, total, accepted) ->
      if meets_threshold ~signed ~total then
        Ok { next = dest; signed_weight = signed; total_weight = total; accepted;
             offered = List.length signatures }
      else Error (Insufficient_weight { signed; total })

type link =
  | Forward of {
      source : block;
      dest : block;
      config_proof : string;
      dest_proof : string;
      signatures : signature list;
    }
  | Backward of { source : block; dest : block }

(* Each link has to start exactly where trust currently reaches. Without that
   check a chain could be handed over out of order, or with a gap in the
   middle, and every individual link would still verify. *)
let follow trusted = function
  | Backward _ -> Error Backward_link
  | Forward l ->
      if not (String.equal l.source.root_hash trusted.root_hash) then
        Error
          (Link_does_not_continue { trusted = hex trusted.root_hash; starts_at = hex l.source.root_hash })
      else
        let* o =
          verify_forward ~from_root_hash:l.source.root_hash ~config_proof:l.config_proof
            ~dest_proof:l.dest_proof ~dest:l.dest ~signatures:l.signatures
        in
        Ok o.next

let follow_all anchor links = List.fold_left (fun acc l -> let* t = acc in follow t l) (Ok anchor) links
