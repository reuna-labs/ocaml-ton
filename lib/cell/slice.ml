(* Reads raise rather than returning a result: a TL-B decoder is a long
   sequence of dependent reads, and threading a result through each one buries
   the structure of the format. [parse] restores totality at the boundary. *)

type error =
  | Not_enough_bits of { want : int; have : int }
  | Not_enough_refs of { have : int }
  | Invalid_width of int
  | Trailing_bits of int
  | Trailing_refs of int
  | Message of string

exception Parse_error of error

let pp_error ppf = function
  | Not_enough_bits { want; have } ->
      Format.fprintf ppf "need %d more bits, %d remain" want have
  | Not_enough_refs { have } -> Format.fprintf ppf "need another reference, %d remain" have
  | Invalid_width n -> Format.fprintf ppf "invalid width %d" n
  | Trailing_bits n -> Format.fprintf ppf "%d bits left unparsed" n
  | Trailing_refs n -> Format.fprintf ppf "%d references left unparsed" n
  | Message m -> Format.pp_print_string ppf m

let fail e = raise (Parse_error e)

type t = { cell : Cell.t; bits : Bits.t; mutable pos : int; mutable ref_pos : int }

let of_cell c = { cell = c; bits = Cell.bits c; pos = 0; ref_pos = 0 }
let copy s = { s with pos = s.pos }
let parse c f = try Ok (f (of_cell c)) with Parse_error e -> Error e
let remaining_bits s = Bits.length s.bits - s.pos
let remaining_refs s = Cell.ref_count s.cell - s.ref_pos
let is_empty s = remaining_bits s = 0 && remaining_refs s = 0
let cell s = s.cell
let to_bits s = Bits.sub s.bits s.pos (remaining_bits s)

let refs s =
  List.filteri (fun i _ -> i >= s.ref_pos) (Cell.refs s.cell)

let need s n =
  if n < 0 then fail (Invalid_width n);
  if n > remaining_bits s then fail (Not_enough_bits { want = n; have = remaining_bits s })

let load_bit s =
  need s 1;
  let v = Bits.get s.bits s.pos in
  s.pos <- s.pos + 1;
  v

let preload_bit s =
  need s 1;
  Bits.get s.bits s.pos

let load_bits s n =
  need s n;
  let v = Bits.sub s.bits s.pos n in
  s.pos <- s.pos + n;
  v

let skip s n =
  need s n;
  s.pos <- s.pos + n

let load_bytes s n =
  let b = load_bits s (8 * n) in
  match Bits.to_bytes b with Some x -> x | None -> fail (Message "load_bytes: not byte-aligned")

let load_uint s ~bits =
  if bits < 0 || bits > 64 then fail (Invalid_width bits);
  need s bits;
  let v = Bits.get_uint s.bits ~pos:s.pos ~len:bits in
  s.pos <- s.pos + bits;
  v

let preload_uint s ~bits =
  if bits < 0 || bits > 64 then fail (Invalid_width bits);
  need s bits;
  Bits.get_uint s.bits ~pos:s.pos ~len:bits

let load_int s ~bits =
  if bits < 0 || bits > 64 then fail (Invalid_width bits);
  need s bits;
  let v = Bits.get_int s.bits ~pos:s.pos ~len:bits in
  s.pos <- s.pos + bits;
  v

let load_uint_z s ~bits =
  if bits < 0 then fail (Invalid_width bits);
  need s bits;
  let acc = ref Z.zero in
  for i = 0 to bits - 1 do
    acc := Z.logor (Z.shift_left !acc 1) (if Bits.get s.bits (s.pos + i) then Z.one else Z.zero)
  done;
  s.pos <- s.pos + bits;
  !acc

let load_int_z s ~bits =
  if bits = 0 then Z.zero
  else begin
    let negative = preload_bit s in
    let v = load_uint_z s ~bits in
    if negative then Z.sub v (Z.shift_left Z.one bits) else v
  end

let load_ref s =
  match Cell.nth_ref s.cell s.ref_pos with
  | None -> fail (Not_enough_refs { have = remaining_refs s })
  | Some c ->
      s.ref_pos <- s.ref_pos + 1;
      c

let load_maybe_ref s = if load_bit s then Some (load_ref s) else None

let end_parse s =
  if remaining_bits s <> 0 then fail (Trailing_bits (remaining_bits s));
  if remaining_refs s <> 0 then fail (Trailing_refs (remaining_refs s))
