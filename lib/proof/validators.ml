open Ton_cell

type validator = { public_key : string; weight : int64; short_id : string }

type t = {
  utime_since : int32;
  utime_until : int32;
  total : int;
  main : int;
  total_weight : int64;
  list : validator list;
}

type error = Elided of string | Malformed of string

let pp_error ppf = function
  | Elided what -> Format.fprintf ppf "the proof does not cover %s" what
  | Malformed m -> Format.pp_print_string ppf m

let sig_pubkey_magic = 0x8e81278al

(* A signature names its validator by the short identifier of its key -- the
   same construction ADNL uses for a peer -- so it is computed once here. *)
let read_validator s =
  let tag = Int64.to_int (Slice.load_uint s ~bits:8) in
  if tag <> 0x53 && tag <> 0x73 then
    Slice.fail (Slice.Message (Printf.sprintf "unknown ValidatorDescr tag %02x" tag));
  let magic = Int64.to_int32 (Slice.load_uint s ~bits:32) in
  if magic <> sig_pubkey_magic then
    Slice.fail (Slice.Message (Printf.sprintf "expected an ed25519 public key, got %08lx" magic));
  let public_key = Slice.load_bytes s 32 in
  let weight = Slice.load_uint s ~bits:64 in
  if tag = 0x73 then ignore (Slice.load_bytes s 32) (* adnl_addr *);
  { public_key; weight; short_id = Ton_adnl_key_id.of_ed25519_pub public_key }

let of_cell c =
  if Cell.is_exotic c then Error (Elided "the validator set")
  else
    match
      Slice.parse c (fun s ->
          let tag = Int64.to_int (Slice.load_uint s ~bits:8) in
          if tag <> 0x11 && tag <> 0x12 then
            Slice.fail (Slice.Message (Printf.sprintf "unknown ValidatorSet tag %02x" tag));
          let utime_since = Int64.to_int32 (Slice.load_uint s ~bits:32) in
          let utime_until = Int64.to_int32 (Slice.load_uint s ~bits:32) in
          let total = Int64.to_int (Slice.load_uint s ~bits:16) in
          let main = Int64.to_int (Slice.load_uint s ~bits:16) in
          (* The plain form stores its list inline and has no precomputed
             weight; the extended form keeps both in a reference. *)
          let stated_weight = if tag = 0x12 then Some (Slice.load_uint s ~bits:64) else None in
          let entries =
            if tag = 0x12 then
              Ton_tlb.Dict.load_maybe s ~key_bits:16 ~value:read_validator
            else Ton_tlb.Dict.load s ~key_bits:16 ~value:read_validator
          in
          let list = List.map snd (Ton_tlb.Dict.to_list entries) in
          let total_weight =
            match stated_weight with
            | Some w -> w
            | None -> List.fold_left (fun acc v -> Int64.add acc v.weight) 0L list
          in
          (Ton_tlb.Dict.is_partial entries, { utime_since; utime_until; total; main; total_weight; list }))
    with
    | Error e -> Error (Malformed (Format.asprintf "%a" Slice.pp_error e))
    | Ok (true, _) -> Error (Elided "part of the validator list")
    | Ok (false, v) -> Ok v

(* The set names [total] validators but only the first [main] sign, so both
   the members that count and the weight they are measured against come from
   that prefix. *)
let main_list t = List.filteri (fun i _ -> i < t.main) t.list
let main_weight t = List.fold_left (fun acc v -> Int64.add acc v.weight) 0L (main_list t)
let find t ~short_id = List.find_opt (fun v -> String.equal v.short_id short_id) (main_list t)
