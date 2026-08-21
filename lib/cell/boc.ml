(* Transliterated from ton-core's parseBoc/deserializeBoc/serializeBoc, with
   the validation made explicit: the reference implementation throws on
   malformed input, whereas this returns a typed error so a unikernel parsing
   liteserver responses cannot be crashed by a hostile peer. *)

type error =
  | Bad_magic of int
  | Truncated of { field : string; want : int; have : int }
  | Unsupported_size of { field : string; got : int }
  | Bad_flags of int
  | Cell_too_large of { index : int; error : Cell.error }
  | Backward_ref of { at : int; target : int }
  | Ref_out_of_range of { at : int; target : int; cells : int }
  | Root_out_of_range of { root : int; cells : int }
  | No_roots
  | Multiple_roots of int
  | Bad_crc of { expected : string; got : string }
  | Trailing_bytes of int

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let pp_error ppf = function
  | Bad_magic m -> Format.fprintf ppf "not a bag of cells: magic 0x%08x" m
  | Truncated { field; want; have } ->
      Format.fprintf ppf "truncated while reading %s: want %d bytes, have %d" field want have
  | Unsupported_size { field; got } -> Format.fprintf ppf "unsupported %s width %d" field got
  | Bad_flags f -> Format.fprintf ppf "reserved flag bits must be zero, got %d" f
  | Cell_too_large { index; error } ->
      Format.fprintf ppf "cell %d is invalid: %a" index Cell.pp_error error
  | Backward_ref { at; target } ->
      Format.fprintf ppf "cell %d references cell %d, which is not strictly forward" at target
  | Ref_out_of_range { at; target; cells } ->
      Format.fprintf ppf "cell %d references cell %d but there are only %d cells" at target cells
  | Root_out_of_range { root; cells } ->
      Format.fprintf ppf "root index %d out of range (%d cells)" root cells
  | No_roots -> Format.fprintf ppf "bag of cells declares no roots"
  | Multiple_roots n -> Format.fprintf ppf "expected exactly one root, got %d" n
  | Bad_crc { expected; got } ->
      Format.fprintf ppf "CRC-32C mismatch: expected %s, computed %s" expected got
  | Trailing_bytes n -> Format.fprintf ppf "%d unexpected trailing bytes" n

exception Fail of error

(* --- reading -------------------------------------------------------------- *)

type reader = { s : string; mutable pos : int }

let remaining r = String.length r.s - r.pos

let need r field n =
  if n < 0 || n > remaining r then raise (Fail (Truncated { field; want = n; have = remaining r }))

let u8 r field =
  need r field 1;
  let v = Char.code r.s.[r.pos] in
  r.pos <- r.pos + 1;
  v

(* Big-endian unsigned integer of [n] bytes. Widths are validated by the
   caller; 7 bytes is the widest that is guaranteed to fit an OCaml int. *)
let uint r field n =
  need r field n;
  let v = ref 0 in
  for i = 0 to n - 1 do
    v := (!v lsl 8) lor Char.code r.s.[r.pos + i]
  done;
  r.pos <- r.pos + n;
  !v

let take r field n =
  need r field n;
  let v = String.sub r.s r.pos n in
  r.pos <- r.pos + n;
  v

let check_width field got max = if got < 1 || got > max then raise (Fail (Unsupported_size { field; got }))

type header = {
  size : int;  (* bytes per cell index *)
  cells : int;
  roots : int list;
  cell_data : string;
}

let verify_crc src =
  (* The checksum covers everything before itself. *)
  let n = String.length src in
  let expected = String.sub src (n - 4) 4 in
  let got = Web3_codec.Crc.crc32c_le (String.sub src 0 (n - 4)) in
  if not (String.equal expected got) then
    raise (Fail (Bad_crc { expected = hex expected; got = hex got }))

let parse_header src =
  let r = { s = src; pos = 0 } in
  let magic = uint r "magic" 4 in
  (* The indexed variants declare [size] in a whole byte; the common variant
     packs it into 3 bits of the flags byte. *)
  let size, off_bytes, has_idx, has_crc, explicit_roots =
    match magic with
    | 0x68ff65f3 ->
        let size = u8 r "size" in
        let off = u8 r "off_bytes" in
        (size, off, true, false, false)
    | 0xacc3a728 ->
        let size = u8 r "size" in
        let off = u8 r "off_bytes" in
        (size, off, true, true, false)
    | 0xb5ee9c72 ->
        let f = u8 r "flags" in
        let has_idx = f land 0x80 <> 0 in
        let has_crc = f land 0x40 <> 0 in
        let reserved = (f lsr 3) land 0x3 in
        if reserved <> 0 then raise (Fail (Bad_flags reserved));
        let size = f land 0x7 in
        let off = u8 r "off_bytes" in
        (size, off, has_idx, has_crc, true)
    | m -> raise (Fail (Bad_magic m))
  in
  check_width "cell index" size 4;
  check_width "offset" off_bytes 7;
  if has_crc then begin
    if String.length src < 4 then raise (Fail (Truncated { field = "crc"; want = 4; have = String.length src }));
    verify_crc src
  end;
  let cells = uint r "cells" size in
  let nroots = uint r "roots" size in
  let _absent = uint r "absent" size in
  let total = uint r "tot_cells_size" off_bytes in
  let roots =
    if explicit_roots then List.init nroots (fun _ -> uint r "root_list" size)
    else [ 0 ]
  in
  (* Every cell costs at least the two descriptor bytes, so this bounds the
     allocation a hostile header can ask for. *)
  if cells < 0 || cells > total / 2 then
    raise (Fail (Truncated { field = "cell_data"; want = cells * 2; have = total }));
  if has_idx then ignore (take r "index" (cells * off_bytes));
  let cell_data = take r "cell_data" total in
  let tail = remaining r - if has_crc then 4 else 0 in
  if tail <> 0 then raise (Fail (Trailing_bytes tail));
  { size; cells; roots; cell_data }

(* One cell's descriptors, data and raw reference indices. *)
let read_cell r ~size =
  let d1 = u8 r "d1" in
  let d2 = u8 r "d2" in
  let refs_count = d1 mod 8 in
  let exotic = d1 land 8 <> 0 in
  let data_bytes = (d2 + 1) / 2 in
  let padded = d2 land 1 = 1 in
  (* Bit 0x10 means the cell carries its own hashes and depths inline; they
     are redundant with what we recompute, so skip them. *)
  if d1 land 0x10 <> 0 then begin
    let n = Level_mask.hash_count (Level_mask.v ((d1 lsr 5) land 7)) in
    ignore (take r "inline hashes" (n * 32));
    ignore (take r "inline depths" (n * 2))
  end;
  let bits =
    if data_bytes = 0 then Bits.empty
    else
      let raw = take r "cell data" data_bytes in
      if padded then Bits.of_padded_bytes raw else Bits.of_bytes raw
  in
  let refs = List.init refs_count (fun _ -> uint r "ref index" size) in
  (bits, refs, exotic)

let deserialize src =
  try
    let h = parse_header src in
    let r = { s = h.cell_data; pos = 0 } in
    let raw = Array.init h.cells (fun _ -> read_cell r ~size:h.size) in
    let extra = remaining r in
    if extra <> 0 then raise (Fail (Trailing_bytes extra));
    (* Build from the back: a reference always points forward, so by the time
       cell [i] is built every cell it needs already exists. *)
    let built = Array.make h.cells Cell.empty in
    for i = h.cells - 1 downto 0 do
      let bits, refs, exotic = raw.(i) in
      let refs =
        List.map
          (fun t ->
            if t < 0 || t >= h.cells then
              raise (Fail (Ref_out_of_range { at = i; target = t; cells = h.cells }));
            if t <= i then raise (Fail (Backward_ref { at = i; target = t }));
            built.(t))
          refs
      in
      match Cell.make ~exotic bits refs with
      | Ok c -> built.(i) <- c
      | Error e -> raise (Fail (Cell_too_large { index = i; error = e }))
    done;
    if h.roots = [] then raise (Fail No_roots);
    Ok
      (List.map
         (fun i ->
           if i < 0 || i >= h.cells then raise (Fail (Root_out_of_range { root = i; cells = h.cells }));
           built.(i))
         h.roots)
  with Fail e -> Error e

let deserialize_root src =
  match deserialize src with
  | Error _ as e -> e
  | Ok [ c ] -> Ok c
  | Ok l -> Error (Multiple_roots (List.length l))

(* --- writing -------------------------------------------------------------- *)

let topological_sort root =
  let seen = Hashtbl.create 64 in
  let order = ref [] in
  (* Depth-first, children right-to-left, prepending on completion. That
     yields reverse post-order -- root first, every reference later -- and
     matches ton-core's ordering byte for byte. *)
  (* Keyed on [Cell.identity], never on the representation hash: a pruned
     branch shares its level-0 hash with the subtree it replaces, so using
     that here would silently collapse the two. *)
  let rec visit c =
    let h = Cell.identity c in
    if not (Hashtbl.mem seen h) then begin
      Hashtbl.add seen h ();
      List.iter visit (List.rev (Cell.refs c));
      order := c :: !order
    end
  in
  visit root;
  let cells = !order in
  let index = Hashtbl.create 64 in
  List.iteri (fun i c -> Hashtbl.replace index (Cell.identity c) i) cells;
  List.map
    (fun c -> (c, List.map (fun r -> Hashtbl.find index (Cell.identity r)) (Cell.refs c)))
    cells

(* Width in bits of an unsigned value, with zero taking one bit -- matching
   the reference implementation, which measures the length of the binary
   string and so never yields zero. *)
let bits_for_uint n =
  let rec go n acc = if n = 0 then acc else go (n lsr 1) (acc + 1) in
  if n = 0 then 1 else go n 0

let width_bytes n = max 1 ((bits_for_uint n + 7) / 8)
let put_uint buf v n = for i = n - 1 downto 0 do Buffer.add_char buf (Char.unsafe_chr ((v lsr (8 * i)) land 0xff)) done

let serialize ?(idx = false) ?(crc32 = false) root =
  let cells = topological_sort root in
  let count = List.length cells in
  let size = width_bytes count in
  let cell_size (c, refs) = 2 + ((Bits.length (Cell.bits c) + 7) / 8) + (List.length refs * size) in
  let total, offsets =
    List.fold_left
      (fun (acc, offs) e ->
        let acc = acc + cell_size e in
        (acc, acc :: offs))
      (0, []) cells
  in
  let offsets = List.rev offsets in
  let off_bytes = width_bytes total in
  let buf = Buffer.create (32 + total + if idx then count * off_bytes else 0) in
  put_uint buf 0xb5ee9c72 4;
  Buffer.add_char buf
    (Char.unsafe_chr (((if idx then 0x80 else 0) lor if crc32 then 0x40 else 0) lor size));
  put_uint buf off_bytes 1;
  put_uint buf count size;
  put_uint buf 1 size (* roots *);
  put_uint buf 0 size (* absent *);
  put_uint buf total off_bytes;
  put_uint buf 0 size (* the root is always index 0 *);
  if idx then List.iter (fun o -> put_uint buf o off_bytes) offsets;
  List.iter
    (fun (c, refs) ->
      let mask = Level_mask.value (Cell.mask c) in
      let typ = Cell.cell_type c in
      let nrefs = List.length refs in
      let d1 = nrefs + (if Cell_type.is_exotic typ then 8 else 0) + (mask * 32) in
      let len = Bits.length (Cell.bits c) in
      let d2 = ((len + 7) / 8) + (len / 8) in
      Buffer.add_char buf (Char.unsafe_chr d1);
      Buffer.add_char buf (Char.unsafe_chr d2);
      Buffer.add_string buf (Bits.to_padded_bytes (Cell.bits c));
      List.iter (fun r -> put_uint buf r size) refs)
    cells;
  if crc32 then Buffer.add_string buf (Web3_codec.Crc.crc32c_le (Buffer.contents buf));
  Buffer.contents buf
