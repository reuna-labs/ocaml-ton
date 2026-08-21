open Ton_cell

type proof = { root : Cell.t; hash : string; depth : int }

type update = {
  old_hash : string;
  new_hash : string;
  old_depth : int;
  new_depth : int;
  old_root : Cell.t;
  new_root : Cell.t;
}

type error = Not_a_proof of Cell_type.t | Not_an_update of Cell_type.t | Malformed of string

let pp_error ppf = function
  | Not_a_proof t -> Format.fprintf ppf "expected a merkle proof cell, got %a" Cell_type.pp t
  | Not_an_update t -> Format.fprintf ppf "expected a merkle update cell, got %a" Cell_type.pp t
  | Malformed m -> Format.pp_print_string ppf m

let read c f = match Slice.parse c f with Ok v -> Ok v | Error e -> Error (Malformed (Format.asprintf "%a" Slice.pp_error e))

let proof c =
  match Cell.cell_type c with
  | Cell_type.Merkle_proof ->
      read c (fun s ->
          ignore (Slice.load_uint s ~bits:8);
          let hash = Slice.load_bytes s 32 in
          let depth = Int64.to_int (Slice.load_uint s ~bits:16) in
          { root = Slice.load_ref s; hash; depth })
  | t -> Error (Not_a_proof t)

let update c =
  match Cell.cell_type c with
  | Cell_type.Merkle_update ->
      read c (fun s ->
          ignore (Slice.load_uint s ~bits:8);
          let old_hash = Slice.load_bytes s 32 in
          let new_hash = Slice.load_bytes s 32 in
          let old_depth = Int64.to_int (Slice.load_uint s ~bits:16) in
          let new_depth = Int64.to_int (Slice.load_uint s ~bits:16) in
          let old_root = Slice.load_ref s in
          let new_root = Slice.load_ref s in
          { old_hash; new_hash; old_depth; new_depth; old_root; new_root })
  | t -> Error (Not_an_update t)
