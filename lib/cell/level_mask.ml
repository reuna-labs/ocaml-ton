type t = int

let v m = m land 0xff
let value m = m

(* 32 - clz32 m, i.e. the position of the highest set bit. *)
let level m =
  let rec go n acc = if n = 0 then acc else go (n lsr 1) (acc + 1) in
  go m 0

let hash_index m =
  let rec go n acc = if n = 0 then acc else go (n lsr 1) (acc + (n land 1)) in
  go m 0

let hash_count m = hash_index m + 1
let apply m l = m land ((1 lsl l) - 1)
let is_significant m l = l = 0 || (m lsr (l - 1)) land 1 <> 0
let equal = Int.equal
let pp ppf m = Format.fprintf ppf "0x%x" m
