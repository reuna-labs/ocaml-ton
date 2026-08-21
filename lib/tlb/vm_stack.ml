(* Not opening Ton_cell here: the constructors below deliberately shadow
   Cell, Slice and Builder, so the modules are reached through aliases. *)
module C = Ton_cell.Cell
module B = Ton_cell.Builder
module S = Ton_cell.Slice
module Bits = Ton_cell.Bits

type item =
  | Null
  | Int of Z.t
  | Nan
  | Cell of C.t
  | Slice of C.t
  | Builder of C.t
  | Tuple of item list

type t = item list

let int64_min = Z.of_string "-9223372036854775808"
let int64_max = Z.of_string "9223372036854775807"

let cell_of b =
  match B.end_cell b with
  | Ok c -> c
  | Error e -> failwith (Format.asprintf "Vm_stack: %a" B.pp_error e)

let rec store_item b = function
  | Null -> B.store_uint b 0x00L ~bits:8
  | Int v ->
      (* The tag depends on the magnitude: small values get the compact form. *)
      if Z.leq v int64_max && Z.geq v int64_min then
        B.store_int_z (B.store_uint b 0x01L ~bits:8) v ~bits:64
      else B.store_int_z (B.store_uint b 0x0100L ~bits:15) v ~bits:257
  | Nan -> B.store_uint b 0x02ffL ~bits:16
  | Cell c -> B.store_ref (B.store_uint b 0x03L ~bits:8) c
  | Slice c ->
      let b = B.store_uint b 0x04L ~bits:8 in
      let b = B.store_uint b 0L ~bits:10 (* st_bits *) in
      let b = B.store_uint b (Int64.of_int (Bits.length (C.bits c))) ~bits:10 (* end_bits *) in
      let b = B.store_uint b 0L ~bits:3 (* st_ref *) in
      let b = B.store_uint b (Int64.of_int (C.ref_count c)) ~bits:3 (* end_ref *) in
      B.store_ref b c
  | Builder c -> B.store_ref (B.store_uint b 0x05L ~bits:8) c
  | Tuple items ->
      (* A balanced-ish spine built by repeatedly folding the previous two
         cells into one. Transliterated from the reference, which is the only
         description of this layout that exists. *)
      let head = ref None and tail = ref None in
      List.iteri
        (fun i it ->
          let swap = !head in
          head := !tail;
          tail := swap;
          if i > 1 then
            head :=
              Some (cell_of (B.store_ref (B.store_ref (B.create ()) (Option.get !tail)) (Option.get !head)));
          tail := Some (cell_of (store_item (B.create ()) it)))
        items;
      let b = B.store_uint b 0x07L ~bits:8 in
      let b = B.store_uint b (Int64.of_int (List.length items)) ~bits:16 in
      let b = match !head with Some c -> B.store_ref b c | None -> b in
      (match !tail with Some c -> B.store_ref b c | None -> b)

let rec load_item cs =
  match Int64.to_int (S.load_uint cs ~bits:8) with
  | 0 -> Null
  | 1 -> Int (S.load_int_z cs ~bits:64)
  | 2 ->
      (* 0x02 is shared: seven more zero bits mean int257, otherwise NaN. *)
      if S.load_uint cs ~bits:7 = 0L then Int (S.load_int_z cs ~bits:257)
      else begin
        ignore (S.load_bit cs);
        Nan
      end
  | 3 -> Cell (S.load_ref cs)
  | 4 ->
      let start_bits = Int64.to_int (S.load_uint cs ~bits:10) in
      let end_bits = Int64.to_int (S.load_uint cs ~bits:10) in
      let start_refs = Int64.to_int (S.load_uint cs ~bits:3) in
      let end_refs = Int64.to_int (S.load_uint cs ~bits:3) in
      if end_bits < start_bits || end_refs < start_refs then
        S.fail (S.Message "vm_stk_slice: end before start");
      (* The referenced cell is a window, not the value itself. *)
      let rs = S.of_cell (S.load_ref cs) in
      S.skip rs start_bits;
      let data = S.load_bits rs (end_bits - start_bits) in
      let b = B.store_bits (B.create ()) data in
      let b = ref b in
      for _ = 1 to start_refs do
        ignore (S.load_ref rs)
      done;
      for _ = 1 to end_refs - start_refs do
        b := B.store_ref !b (S.load_ref rs)
      done;
      Slice (cell_of !b)
  | 5 -> Builder (S.load_ref cs)
  | 7 ->
      let len = Int64.to_int (S.load_uint cs ~bits:16) in
      if len > 1 then begin
        let head = ref (S.of_cell (S.load_ref cs)) in
        let tail = ref (S.of_cell (S.load_ref cs)) in
        let acc = ref [ load_item !tail ] in
        for _ = 1 to len - 2 do
          let ohead = !head in
          head := S.of_cell (S.load_ref ohead);
          tail := S.of_cell (S.load_ref ohead);
          acc := load_item !tail :: !acc
        done;
        Tuple (load_item !head :: !acc)
      end
      else if len = 1 then Tuple [ load_item (S.of_cell (S.load_ref cs)) ]
      else Tuple []
  | k -> S.fail (S.Message (Printf.sprintf "unsupported stack item tag %d" k))

(* vm_stk_cons puts the rest of the stack in a reference and the top inline,
   so the list is written from the bottom up. *)
let rec store_tail b = function
  | [] -> b
  | items ->
      let rest, top =
        match List.rev items with top :: rev_rest -> (List.rev rev_rest, top) | [] -> assert false
      in
      let b = B.store_ref b (cell_of (store_tail (B.create ()) rest)) in
      store_item b top

let to_cell stack =
  (* [store_tail] puts the last element of its argument inline at the
     outermost level. The bottom of the stack is the outermost item, so the
     list is passed top-first exactly as held -- reversing here would produce
     a stack that round-trips against itself but not against anyone else. *)
  try
    let b = B.store_uint (B.create ()) (Int64.of_int (List.length stack)) ~bits:24 in
    match B.end_cell (store_tail b stack) with
    | Ok c -> Ok c
    | Error e -> Error (Format.asprintf "%a" B.pp_error e)
  with Failure m -> Error m

let of_cell c =
  S.parse c (fun s ->
      let depth = Int64.to_int (S.load_uint s ~bits:24) in
      let cs = ref s and acc = ref [] in
      for _ = 1 to depth do
        let next = S.load_ref !cs in
        acc := load_item !cs :: !acc;
        cs := S.of_cell next
      done;
      (* The outermost item is the bottom of the stack and is read first, so
         prepending as we descend leaves the top at the head. *)
      !acc)

let rec equal a b =
  match (a, b) with
  | Null, Null | Nan, Nan -> true
  | Int x, Int y -> Z.equal x y
  | Cell x, Cell y | Slice x, Slice y | Builder x, Builder y -> C.equal x y
  | Tuple x, Tuple y -> List.length x = List.length y && List.for_all2 equal x y
  | _ -> false

let rec pp ppf = function
  | Null -> Format.pp_print_string ppf "null"
  | Int v -> Format.pp_print_string ppf (Z.to_string v)
  | Nan -> Format.pp_print_string ppf "nan"
  | Cell c -> Format.fprintf ppf "cell(%s)" (String.sub (Ton_cell.Cell.hash c |> fun s -> String.concat "" (List.init 4 (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))) 0 8)
  | Slice _ -> Format.pp_print_string ppf "slice"
  | Builder _ -> Format.pp_print_string ppf "builder"
  | Tuple l -> Format.fprintf ppf "[%a]" (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ", ") pp) l

let pp_stack ppf l =
  Format.fprintf ppf "@[<hov 2>%a@]" (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f " | ") pp) l
