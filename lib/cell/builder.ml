(* A cell holds at most 1023 bits, so the data buffer is a fixed 128 bytes and
   never grows.

   Writes do not return a result. Threading an error through every store in a
   TL-B serializer would dominate the code for a condition that indicates a
   programming mistake rather than bad input, so the first overflow is latched
   and surfaced by [end_cell]. Once latched, further writes are dropped, which
   keeps the buffer bounds trivially safe. *)

type error =
  | Bit_overflow of { have : int; want : int }
  | Ref_overflow of { have : int }
  | Invalid_width of int
  | Cell of Cell.error

let pp_error ppf = function
  | Bit_overflow { have; want } ->
      Format.fprintf ppf "cannot store %d bits: only %d of %d remain" want have Cell.max_bits
  | Ref_overflow { have } ->
      Format.fprintf ppf "cannot store a reference: %d of %d already stored" have Cell.max_refs
  | Invalid_width n -> Format.fprintf ppf "invalid width %d" n
  | Cell e -> Cell.pp_error ppf e

type t = {
  data : Bytes.t;
  mutable len : int;
  mutable refs : Cell.t list; (* reversed *)
  mutable nrefs : int;
  mutable err : error option;
}

let create () =
  { data = Bytes.make ((Cell.max_bits + 7) / 8) '\000'; len = 0; refs = []; nrefs = 0; err = None }

let bit_length b = b.len
let ref_count b = b.nrefs
let available_bits b = Cell.max_bits - b.len
let available_refs b = Cell.max_refs - b.nrefs
let error b = b.err
let fail b e = if b.err = None then b.err <- Some e

let put_bit b v =
  let i = b.len lsr 3 and k = 7 - (b.len land 7) in
  let cur = Char.code (Bytes.unsafe_get b.data i) in
  Bytes.unsafe_set b.data i
    (Char.unsafe_chr (if v then cur lor (1 lsl k) else cur land lnot (1 lsl k)));
  b.len <- b.len + 1

(* Every write funnels through here, so the capacity check exists once. *)
let reserve b n =
  if b.err <> None then false
  else if n > available_bits b then begin
    fail b (Bit_overflow { have = available_bits b; want = n });
    false
  end
  else true

let store_bit b v =
  if reserve b 1 then put_bit b v;
  b

let store_bits b bits =
  let n = Bits.length bits in
  if reserve b n then
    for i = 0 to n - 1 do
      put_bit b (Bits.get bits i)
    done;
  b

let store_bytes b s = store_bits b (Bits.of_bytes s)

let store_uint b v ~bits =
  if bits < 0 || bits > 64 then fail b (Invalid_width bits)
  else if reserve b bits then
    for i = bits - 1 downto 0 do
      put_bit b (Int64.logand (Int64.shift_right_logical v i) 1L <> 0L)
    done;
  b

let store_int b v ~bits = store_uint b v ~bits

let store_uint_z b v ~bits =
  if bits < 0 then fail b (Invalid_width bits)
  else if Z.sign v < 0 then fail b (Invalid_width bits)
  else if reserve b bits then
    for i = bits - 1 downto 0 do
      put_bit b (Z.testbit v i)
    done;
  b

let store_int_z b v ~bits =
  if bits < 0 then fail b (Invalid_width bits)
  else if reserve b bits then begin
    (* Two's complement: for negatives, [Z.testbit] on the infinite-precision
       representation already yields the right bits. *)
    for i = bits - 1 downto 0 do
      put_bit b (Z.testbit v i)
    done
  end;
  b

let store_ref b c =
  if b.err = None then
    if b.nrefs >= Cell.max_refs then fail b (Ref_overflow { have = b.nrefs })
    else begin
      b.refs <- c :: b.refs;
      b.nrefs <- b.nrefs + 1
    end;
  b

let store_maybe_ref b = function
  | None -> store_bit b false
  | Some c -> store_ref (store_bit b true) c

let to_bits b = Bits.sub (Bits.of_bytes (Bytes.to_string b.data)) 0 b.len

let store_builder b other =
  (match other.err with Some e -> fail b e | None -> ());
  let b = store_bits b (to_bits other) in
  List.fold_left store_ref b (List.rev other.refs)

let store_slice b s = List.fold_left store_ref (store_bits b (Slice.to_bits s)) (Slice.refs s)

let end_cell ?(exotic = false) b =
  match b.err with
  | Some e -> Error e
  | None -> (
      match Cell.make ~exotic (to_bits b) (List.rev b.refs) with
      | Ok c -> Ok c
      | Error e -> Error (Cell e))
