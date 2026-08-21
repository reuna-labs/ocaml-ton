open Ton_cell

type error = Negative of Z.t | Too_large of { value : Z.t; max_bytes : int }

let pp_error ppf = function
  | Negative v -> Format.fprintf ppf "amount %s is negative" (Z.to_string v)
  | Too_large { value; max_bytes } ->
      Format.fprintf ppf "amount %s does not fit in %d bytes" (Z.to_string value) max_bytes

(* Width of the [len] field: [#< n] holds values 0..n-1. *)
let len_width n =
  let rec go n acc = if n = 0 then acc else go (n lsr 1) (acc + 1) in
  go (n - 1) 0

let load_var_uint s ~n =
  let len = Int64.to_int (Slice.load_uint s ~bits:(len_width n)) in
  if len = 0 then Z.zero else Slice.load_uint_z s ~bits:(len * 8)

let store_var_uint b ~n v =
  if Z.sign v < 0 then Error (Negative v)
  else
    let bytes = (Z.numbits v + 7) / 8 in
    if bytes > n - 1 then Error (Too_large { value = v; max_bytes = n - 1 })
    else
      let b = Builder.store_uint b (Int64.of_int bytes) ~bits:(len_width n) in
      Ok (if bytes = 0 then b else Builder.store_uint_z b v ~bits:(bytes * 8))

let load_coins s = load_var_uint s ~n:16
let store_coins b v = store_var_uint b ~n:16 v

let nano_per_ton = Z.of_int 1_000_000_000

let to_string v =
  let sign = if Z.sign v < 0 then "-" else "" in
  let v = Z.abs v in
  let whole = Z.div v nano_per_ton and frac = Z.rem v nano_per_ton in
  if Z.equal frac Z.zero then sign ^ Z.to_string whole
  else
    (* Nine digits, zero-padded on the left. *)
    let d = Z.to_string frac in
    let f = String.make (9 - String.length d) '0' ^ d in
    let rec trim i = if i > 0 && f.[i - 1] = '0' then trim (i - 1) else i in
    sign ^ Z.to_string whole ^ "." ^ String.sub f 0 (trim (String.length f))

let of_string s =
  let s = String.trim s in
  let negative = String.length s > 0 && s.[0] = '-' in
  let s = if negative || (String.length s > 0 && s.[0] = '+') then String.sub s 1 (String.length s - 1) else s in
  let whole, frac =
    match String.index_opt s '.' with
    | None -> (s, "")
    | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  in
  if whole = "" && frac = "" then Error "empty amount"
  else if String.length frac > 9 then
    Error (Printf.sprintf "%d decimal places, but a nanoton is 10^-9" (String.length frac))
  else if not (String.for_all (fun c -> c >= '0' && c <= '9') (whole ^ frac)) then
    Error (Printf.sprintf "not a decimal amount: %S" s)
  else
    let padded = frac ^ String.make (9 - String.length frac) '0' in
    let v = Z.add (Z.mul (if whole = "" then Z.zero else Z.of_string whole) nano_per_ton)
              (if padded = "" then Z.zero else Z.of_string padded) in
    Ok (if negative then Z.neg v else v)
