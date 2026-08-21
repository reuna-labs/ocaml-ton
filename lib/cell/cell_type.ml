type t = Ordinary | Pruned_branch | Library | Merkle_proof | Merkle_update

let to_tag = function
  | Ordinary -> -1
  | Pruned_branch -> 1
  | Library -> 2
  | Merkle_proof -> 3
  | Merkle_update -> 4

let of_tag = function
  | 1 -> Some Pruned_branch
  | 2 -> Some Library
  | 3 -> Some Merkle_proof
  | 4 -> Some Merkle_update
  | _ -> None

let is_exotic t = t <> Ordinary
let equal = ( = )

let pp ppf = function
  | Ordinary -> Format.pp_print_string ppf "ordinary"
  | Pruned_branch -> Format.pp_print_string ppf "pruned_branch"
  | Library -> Format.pp_print_string ppf "library"
  | Merkle_proof -> Format.pp_print_string ppf "merkle_proof"
  | Merkle_update -> Format.pp_print_string ppf "merkle_update"
