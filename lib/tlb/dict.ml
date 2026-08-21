(* Transliterated from ton-core's parseDict/serializeDict.

   The label-length field for the long and same forms is [ceil(log2(n+1))]
   bits wide. That is exactly the bit length of [n], computed here with
   integers rather than logarithms so there is no floating-point edge case at
   powers of two. *)

open Ton_cell

module ZMap = Map.Make (Z)

type 'v t = { key_bits : int; entries : 'v ZMap.t; partial : bool }

let empty ~key_bits = { key_bits; entries = ZMap.empty; partial = false }
let key_bits d = d.key_bits
let cardinal d = ZMap.cardinal d.entries
let is_empty d = ZMap.is_empty d.entries
let is_partial d = d.partial
let find k d = ZMap.find_opt k d.entries
let mem k d = ZMap.mem k d.entries
let add k v d = { d with entries = ZMap.add k v d.entries }
let remove k d = { d with entries = ZMap.remove k d.entries }
let fold f d init = ZMap.fold f d.entries init
let map f d = { d with entries = ZMap.map f d.entries }
let to_list d = ZMap.bindings d.entries
let of_list ~key_bits l = { key_bits; entries = ZMap.of_seq (List.to_seq l); partial = false }

(* Width of the length field in hml_long / hml_same. *)
let label_len_width n =
  let rec go n acc = if n = 0 then acc else go (n lsr 1) (acc + 1) in
  go n 0

(* --- reading -------------------------------------------------------------- *)

let read_unary_length s =
  let n = ref 0 in
  while Slice.load_bit s do
    incr n
  done;
  !n

let append_bit key b = Z.add (Z.shift_left key 1) (if b then Z.one else Z.zero)

(* Reads one edge label and returns its length and the bits it contributed.
   Shared by the full traversal and the single-key walk, so the three label
   forms are decoded in exactly one place. *)
let read_label s ~n ~key =
  if not (Slice.load_bit s) then begin
    (* hml_short *)
    let l = read_unary_length s in
    if l > n then Slice.fail (Slice.Message "hashmap label longer than the remaining key");
    let k = ref key in
    for _ = 1 to l do
      k := append_bit !k (Slice.load_bit s)
    done;
    (l, !k)
  end
  else
    let w = label_len_width n in
    if not (Slice.load_bit s) then begin
      (* hml_long *)
      let l = Int64.to_int (Slice.load_uint s ~bits:w) in
      if l > n then Slice.fail (Slice.Message "hashmap label longer than the remaining key");
      let k = ref key in
      for _ = 1 to l do
        k := append_bit !k (Slice.load_bit s)
      done;
      (l, !k)
    end
    else begin
      (* hml_same *)
      let b = Slice.load_bit s in
      let l = Int64.to_int (Slice.load_uint s ~bits:w) in
      if l > n then Slice.fail (Slice.Message "hashmap label longer than the remaining key");
      let k = ref key in
      for _ = 1 to l do
        k := append_bit !k b
      done;
      (l, !k)
    end

let rec do_parse s ~n ~key ~value ~acc ~partial =
  let prefix_len, key = read_label s ~n ~key in
  let rest = n - prefix_len in
  if rest = 0 then acc := ZMap.add key (value s) !acc
  else begin
    (* A fork's two references implicitly carry the bits 0 and 1. *)
    let left = Slice.load_ref s in
    let right = Slice.load_ref s in
    let descend cell bit =
      (* Inside a Merkle proof an entire subtree may be replaced by a pruned
         branch. Skip it, but remember that the result is incomplete. *)
      if Cell.is_exotic cell then partial := true
      else do_parse (Slice.of_cell cell) ~n:(rest - 1) ~key:(append_bit key bit) ~value ~acc ~partial
    in
    descend left false;
    descend right true
  end

let load s ~key_bits ~value =
  let acc = ref ZMap.empty and partial = ref false in
  do_parse s ~n:key_bits ~key:Z.zero ~value ~acc ~partial;
  { key_bits; entries = !acc; partial = !partial }

let load_maybe s ~key_bits ~value =
  if not (Slice.load_bit s) then empty ~key_bits
  else
    let root = Slice.load_ref s in
    if Cell.is_exotic root then { (empty ~key_bits) with partial = true }
    else load (Slice.of_cell root) ~key_bits ~value

(* --- single-key lookup ------------------------------------------------------ *)

type 'v lookup = Found of 'v | Absent | Elided

exception Pruned

(* Follows one key's path instead of decoding the whole map.

   The third outcome is the point. Inside a Merkle proof a subtree may have
   been replaced by a pruned branch, and then "this key is not in the map" and
   "I was not shown the part of the map where it would be" are different
   claims. Conflating them would let a server deny the existence of anything
   it chose not to include. *)
let lookup_generic cell ~key_bits ~key ~leaf =
  if Z.sign key < 0 || Z.numbits key > key_bits then invalid_arg "Dict.lookup: key does not fit";
  (* The [len] bits of the key starting at [from], as an integer, so it can be
     compared with the label the edge just yielded. *)
  let sub_bits from len =
    let acc = ref Z.zero in
    for i = 0 to len - 1 do
      acc := append_bit !acc (Z.testbit key (key_bits - 1 - (from + i)))
    done;
    !acc
  in
  let rec go cell ~consumed =
    if Cell.is_exotic cell then raise Pruned;
    let s = Slice.of_cell cell in
    let len, label = read_label s ~n:(key_bits - consumed) ~key:Z.zero in
    if not (Z.equal label (sub_bits consumed len)) then Absent
    else
      let consumed = consumed + len in
      if consumed = key_bits then Found (leaf s)
      else begin
        (* The two references stand for the next bit being 0 and 1. *)
        let left = Slice.load_ref s in
        let right = Slice.load_ref s in
        let next = if Z.testbit key (key_bits - 1 - consumed) then right else left in
        go next ~consumed:(consumed + 1)
      end
  in
  try Ok (go cell ~consumed:0) with
  | Pruned -> Ok Elided
  | Slice.Parse_error e -> Error e

let lookup cell ~key_bits ~key ~value = lookup_generic cell ~key_bits ~key ~leaf:value

(* An augmented hashmap carries an [extra] alongside every node. At a leaf it
   precedes the value and must be skipped to reach it; at a fork it follows
   the two references and never has to be read at all, because descending only
   needs the references. *)
let lookup_aug cell ~key_bits ~key ~extra ~value =
  lookup_generic cell ~key_bits ~key ~leaf:(fun s ->
      ignore (extra s);
      value s)

let of_cell c ~key_bits ~value =
  Slice.parse c (fun s ->
      let d = load s ~key_bits ~value in
      Slice.end_parse s;
      d)

(* --- writing -------------------------------------------------------------- *)

type error = Empty_hashmap | Key_out_of_range of Z.t | Builder of Builder.error

let pp_error ppf = function
  | Empty_hashmap -> Format.fprintf ppf "an empty dictionary has no Hashmap encoding; use store_maybe"
  | Key_out_of_range k -> Format.fprintf ppf "key %s does not fit the dictionary's key width" (Z.to_string k)
  | Builder e -> Builder.pp_error ppf e

exception Fail of error

(* Keys become fixed-width bit strings so prefix work is ordinary string
   handling, mirroring the reference implementation. *)
let key_to_string ~key_bits k =
  if Z.sign k < 0 || Z.numbits k > key_bits then raise (Fail (Key_out_of_range k));
  String.init key_bits (fun i -> if Z.testbit k (key_bits - 1 - i) then '1' else '0')

type 'v node = Leaf of 'v | Fork of 'v edge * 'v edge
and 'v edge = { label : string; node : 'v node }

let common_prefix keys start =
  match keys with
  | [] -> ""
  | first :: rest ->
      let len = ref (String.length first - start) in
      List.iter
        (fun k ->
          let i = ref 0 in
          while !i < !len && k.[start + !i] = first.[start + !i] do
            incr i
          done;
          len := !i)
        rest;
      String.sub first start !len

let rec build_edge entries prefix_len =
  let label = common_prefix (List.map fst entries) prefix_len in
  { label; node = build_node entries (prefix_len + String.length label) }

and build_node entries prefix_len =
  match entries with
  | [ (_, v) ] -> Leaf v
  | _ ->
      let left, right = List.partition (fun (k, _) -> k.[prefix_len] = '0') entries in
      (* Both sides are non-empty: the label consumed every shared bit, so the
         next bit must differ across the group. *)
      Fork (build_edge left (prefix_len + 1), build_edge right (prefix_len + 1))

let bits_of_label s =
  let n = String.length s in
  let b = Bytes.make ((n + 7) / 8) '\000' in
  String.iteri
    (fun i c ->
      if c = '1' then
        Bytes.set b (i lsr 3) (Char.chr (Char.code (Bytes.get b (i lsr 3)) lor (1 lsl (7 - (i land 7))))))
    s;
  Bits.sub (Bits.of_bytes (Bytes.to_string b)) 0 n

let is_same s = String.length s <= 1 || String.for_all (fun c -> c = s.[0]) s

(* All three encodings are costed and the cheapest wins; ties go to the
   earlier form. This choice is part of the canonical encoding, so getting it
   wrong changes the cell hash rather than merely wasting bits. *)
let choose_label label key_len =
  let len = String.length label in
  let w = label_len_width key_len in
  let short = (2 * len) + 2 and long = 2 + w + len and same = 3 + w in
  let kind, best = if long < short then (`Long, long) else (`Short, short) in
  if is_same label && same < best then `Same else kind

let write_label to_ label key_len =
  let len = String.length label in
  let w = label_len_width key_len in
  match choose_label label key_len with
  | `Short ->
      (* 0, then the length in unary, then the bits themselves. *)
      let b = ref (Builder.store_bit to_ false) in
      String.iter (fun _ -> b := Builder.store_bit !b true) label;
      Builder.store_bits (Builder.store_bit !b false) (bits_of_label label)
  | `Long ->
      let b = Builder.store_bit (Builder.store_bit to_ true) false in
      let b = Builder.store_uint b (Int64.of_int len) ~bits:w in
      Builder.store_bits b (bits_of_label label)
  | `Same ->
      let b = Builder.store_bit (Builder.store_bit to_ true) true in
      let b = Builder.store_bit b (label <> "" && label.[0] = '1') in
      Builder.store_uint b (Int64.of_int len) ~bits:w

let rec write_edge to_ edge key_len ~value =
  let to_ = write_label to_ edge.label key_len in
  write_node to_ edge.node (key_len - String.length edge.label) ~value

and write_node to_ node key_len ~value =
  match node with
  | Leaf v -> value to_ v
  | Fork (l, r) ->
      let child e =
        match Builder.end_cell (write_edge (Builder.create ()) e (key_len - 1) ~value) with
        | Ok c -> c
        | Error e -> raise (Fail (Builder e))
      in
      let lc = child l in
      let rc = child r in
      Builder.store_ref (Builder.store_ref to_ lc) rc

let store b ~value d =
  try
    if is_empty d then Error Empty_hashmap
    else
      let entries = List.map (fun (k, v) -> (key_to_string ~key_bits:d.key_bits k, v)) (to_list d) in
      Ok (write_edge b (build_edge entries 0) d.key_bits ~value)
  with Fail e -> Error e

let store_maybe b ~value d =
  if is_empty d then Ok (Builder.store_bit b false)
  else
    match store (Builder.create ()) ~value d with
    | Error e -> Error e
    | Ok root -> (
        match Builder.end_cell root with
        | Error e -> Error (Builder e)
        | Ok c -> Ok (Builder.store_ref (Builder.store_bit b true) c))

let to_cell ~value d =
  match store (Builder.create ()) ~value d with
  | Error e -> Error e
  | Ok b -> ( match Builder.end_cell b with Ok c -> Ok c | Error e -> Error (Builder e))
