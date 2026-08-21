(* Backing store is a plain string plus a bit-range. Slicing is therefore
   allocation-free; only [concat]/[append] copy. *)
type t = { data : string; off : int; len : int }

let empty = { data = ""; off = 0; len = 0 }
let length t = t.len
let is_empty t = t.len = 0

let byte s i = Char.code (String.unsafe_get s i)

let unsafe_get t i =
  let p = t.off + i in
  byte t.data (p lsr 3) lsr (7 - (p land 7)) land 1 = 1

let get t i =
  if i < 0 || i >= t.len then invalid_arg "Bits.get: index out of bounds";
  unsafe_get t i

let sub t pos len =
  if pos < 0 || len < 0 || pos + len > t.len then
    invalid_arg "Bits.sub: range out of bounds";
  { data = t.data; off = t.off + pos; len }

(* Writing into a Bytes.t at an arbitrary bit offset. Used by [concat] and by
   [Builder]; kept here so the shifting logic lives in exactly one place. *)
let blit_bits src ~src_pos dst ~dst_pos ~len =
  for i = 0 to len - 1 do
    let p = dst_pos + i in
    let idx = p lsr 3 and bit = 7 - (p land 7) in
    let cur = Char.code (Bytes.unsafe_get dst idx) in
    let v = if unsafe_get src (src_pos + i) then cur lor (1 lsl bit)
            else cur land lnot (1 lsl bit) in
    Bytes.unsafe_set dst idx (Char.unsafe_chr v)
  done

let concat ts =
  let total = List.fold_left (fun acc t -> acc + t.len) 0 ts in
  if total = 0 then empty
  else begin
    let buf = Bytes.make ((total + 7) / 8) '\000' in
    let _ =
      List.fold_left
        (fun pos t -> blit_bits t ~src_pos:0 buf ~dst_pos:pos ~len:t.len; pos + t.len)
        0 ts
    in
    { data = Bytes.unsafe_to_string buf; off = 0; len = total }
  end

let append a b = concat [ a; b ]

let get_uint t ~pos ~len =
  if len < 0 || len > 64 then invalid_arg "Bits.get_uint: length must be 0..64";
  if pos < 0 || pos + len > t.len then
    invalid_arg "Bits.get_uint: range out of bounds";
  let acc = ref 0L in
  for i = 0 to len - 1 do
    let b = if unsafe_get t (pos + i) then 1L else 0L in
    acc := Int64.logor (Int64.shift_left !acc 1) b
  done;
  !acc

let get_int t ~pos ~len =
  if len = 0 then 0L
  else begin
    let v = get_uint t ~pos ~len in
    (* Sign-extend from bit [len-1]. *)
    if len = 64 then v
    else if Int64.logand v (Int64.shift_left 1L (len - 1)) = 0L then v
    else Int64.sub v (Int64.shift_left 1L len)
  end

let of_bytes s = { data = s; off = 0; len = 8 * String.length s }

let to_bytes t =
  if t.len land 7 <> 0 then None
  else if t.off land 7 = 0 then Some (String.sub t.data (t.off lsr 3) (t.len lsr 3))
  else begin
    let buf = Bytes.make (t.len lsr 3) '\000' in
    blit_bits t ~src_pos:0 buf ~dst_pos:0 ~len:t.len;
    Some (Bytes.unsafe_to_string buf)
  end

let to_padded_bytes t =
  match to_bytes t with
  | Some s -> s
  | None ->
      (* Not byte-aligned, so there is always room for the tag bit in the
         final partial byte. *)
      let n = (t.len + 7) / 8 in
      let buf = Bytes.make n '\000' in
      blit_bits t ~src_pos:0 buf ~dst_pos:0 ~len:t.len;
      let idx = t.len lsr 3 and bit = 7 - (t.len land 7) in
      Bytes.unsafe_set buf idx
        (Char.unsafe_chr (Char.code (Bytes.unsafe_get buf idx) lor (1 lsl bit)));
      Bytes.unsafe_to_string buf

let of_padded_bytes s =
  let n = String.length s in
  let rec last i = if i < 0 then None else if byte s i <> 0 then Some i else last (i - 1) in
  match last (n - 1) with
  | None -> empty
  | Some i ->
      (* Lowest set bit of the last non-zero byte is the completion tag. *)
      let b = byte s i in
      let rec low k = if b lsr k land 1 = 1 then k else low (k + 1) in
      let k = low 0 in
      { data = s; off = 0; len = (i * 8) + (7 - k) }

let equal a b =
  a.len = b.len
  &&
  let rec go i = i >= a.len || (unsafe_get a i = unsafe_get b i && go (i + 1)) in
  go 0

let compare a b =
  let n = min a.len b.len in
  let rec go i =
    if i >= n then Stdlib.compare a.len b.len
    else
      match Stdlib.compare (unsafe_get a i) (unsafe_get b i) with
      | 0 -> go (i + 1)
      | c -> c
  in
  go 0

let pp ppf t =
  (* TON's hex-with-underscore notation: byte-aligned values print as plain
     hex, others print the padded form with a trailing '_'. *)
  let s = to_padded_bytes t in
  String.iter (fun c -> Format.fprintf ppf "%02X" (Char.code c)) s;
  if t.len land 7 <> 0 then Format.pp_print_char ppf '_'
