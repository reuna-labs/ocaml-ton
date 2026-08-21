open Ton_tlb
module C = Ton_cell.Cell
module B = Ton_cell.Builder
module Boc = Ton_cell.Boc

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let expected =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/vmstack-expected.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "vmstack-expected.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_int = function `Int n -> n | _ -> Alcotest.fail "expected an int"
let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"
let cell_of b = match B.end_cell b with Ok c -> c | Error e -> Alcotest.failf "%a" B.pp_error e

let cell_a = cell_of (B.store_uint (B.create ()) 0xdeadbeefL ~bits:32)
let cell_b = cell_of (B.store_ref (B.store_uint (B.create ()) 1L ~bits:8) cell_a)
let z = Z.of_string
let ints l = List.map (fun v -> Vm_stack.Int (z v)) l

(* The same stacks the generator builds, by name. *)
let cases : (string * Vm_stack.t) list =
  [ ("empty", []);
    ("null_only", [ Vm_stack.Null ]);
    ("small_int", ints [ "42" ]);
    ("negative_int", ints [ "-42" ]);
    ("int64_max", ints [ "9223372036854775807" ]);
    ("int64_min", ints [ "-9223372036854775808" ]);
    ("just_over_max", ints [ "9223372036854775808" ]);
    ("just_under_min", ints [ "-9223372036854775809" ]);
    ("int257_max", [ Vm_stack.Int (Z.sub (Z.shift_left Z.one 256) Z.one) ]);
    ("int257_min", [ Vm_stack.Int (Z.neg (Z.shift_left Z.one 256)) ]);
    ("nan", [ Vm_stack.Nan ]);
    ("cell", [ Vm_stack.Cell cell_a ]);
    ("builder", [ Vm_stack.Builder cell_a ]);
    ("slice", [ Vm_stack.Slice cell_a ]);
    ("slice_with_refs", [ Vm_stack.Slice cell_b ]);
    ("mixed", [ Vm_stack.Int Z.one; Vm_stack.Null; Vm_stack.Cell cell_a ]);
    ("deep", List.init 20 (fun i -> Vm_stack.Int (Z.of_int i)));
    ("tuple_empty", [ Vm_stack.Tuple [] ]);
    ("tuple_one", [ Vm_stack.Tuple (ints [ "7" ]) ]);
    ("tuple_two", [ Vm_stack.Tuple (ints [ "1"; "2" ]) ]);
    ("tuple_three", [ Vm_stack.Tuple (ints [ "1"; "2" ] @ [ Vm_stack.Null ]) ]);
    ("tuple_seven", [ Vm_stack.Tuple (List.init 7 (fun i -> Vm_stack.Int (Z.of_int (i * 100)))) ]);
    ( "tuple_nested",
      [ Vm_stack.Tuple
          [ Vm_stack.Int Z.one; Vm_stack.Tuple (ints [ "2"; "3" ]); Vm_stack.Cell cell_a ] ] )
  ]

let case (name, stack) () =
  let spec = to_obj (field name (Lazy.force expected)) in
  let c = match Vm_stack.to_cell stack with Ok c -> c | Error e -> Alcotest.failf "%s: %s" name e in
  let at f = Printf.sprintf "%s: %s" name f in
  Alcotest.(check string) (at "hash") (to_str (field "hash" spec)) (hex (C.hash c));
  Alcotest.(check int) (at "bits") (to_int (field "bits" spec)) (Ton_cell.Bits.length (C.bits c));
  Alcotest.(check int) (at "refs") (to_int (field "refs" spec)) (C.ref_count c);
  Alcotest.(check string) (at "boc")
    (to_str (field "boc" spec))
    (Base64.encode_string (Boc.serialize c));
  (* Reading it back must give the same stack, and re-encoding must be stable. *)
  match Vm_stack.of_cell c with
  | Error e -> Alcotest.failf "%s: reparse: %a" name Ton_cell.Slice.pp_error e
  | Ok back ->
      Alcotest.(check int) (at "depth") (List.length stack) (List.length back);
      Alcotest.(check bool) (at "items survive") true
        (List.length stack = List.length back && List.for_all2 Vm_stack.equal stack back);
      let c2 = Result.get_ok (Vm_stack.to_cell back) in
      Alcotest.(check string) (at "re-encoding is stable") (hex (C.hash c)) (hex (C.hash c2))

(* The stack is stored bottom-up through references, so getting the direction
   wrong would still round-trip if the list were symmetric. Use an asymmetric
   one and check the top explicitly. *)
let test_order () =
  let stack = ints [ "1"; "2"; "3" ] in
  let c = Result.get_ok (Vm_stack.to_cell stack) in
  match Vm_stack.of_cell c with
  | Ok [ Vm_stack.Int a; Vm_stack.Int b; Vm_stack.Int d ] ->
      Alcotest.(check string) "top of stack first" "1" (Z.to_string a);
      Alcotest.(check string) "middle" "2" (Z.to_string b);
      Alcotest.(check string) "bottom" "3" (Z.to_string d)
  | Ok other -> Alcotest.failf "unexpected stack of %d items" (List.length other)
  | Error e -> Alcotest.failf "%a" Ton_cell.Slice.pp_error e

(* An integer's tag depends on its magnitude, so a value that crosses the
   int64 boundary must still come back equal. *)
let test_int_boundary () =
  List.iter
    (fun v ->
      let stack = [ Vm_stack.Int (z v) ] in
      let c = Result.get_ok (Vm_stack.to_cell stack) in
      match Vm_stack.of_cell c with
      | Ok [ Vm_stack.Int back ] -> Alcotest.(check string) ("roundtrip " ^ v) v (Z.to_string back)
      | Ok _ -> Alcotest.failf "%s: wrong shape" v
      | Error e -> Alcotest.failf "%s: %a" v Ton_cell.Slice.pp_error e)
    [ "0"; "-1"; "9223372036854775807"; "9223372036854775808"; "-9223372036854775808";
      "-9223372036854775809" ]

let test_rejects_unknown_tag () =
  (* A well-formed one-item stack whose item carries a tag nothing defines:
     the reference for the rest of the list has to be present, or the parser
     would trip over that first. *)
  let b = B.store_uint (B.create ()) 1L ~bits:24 in
  let b = B.store_ref b (cell_of (B.create ())) in
  let c = cell_of (B.store_uint b 0x09L ~bits:8) in
  match Vm_stack.of_cell c with
  | Ok _ -> Alcotest.fail "accepted an unknown stack item tag"
  | Error e ->
      Alcotest.(check string) "error" "unsupported stack item tag 9"
        (Format.asprintf "%a" Ton_cell.Slice.pp_error e)

let () =
  Alcotest.run "vm_stack"
    [ ("vs reference", List.map (fun (n, s) -> Alcotest.test_case n `Quick (case (n, s))) cases);
      ( "behaviour",
        [ Alcotest.test_case "stack order" `Quick test_order;
          Alcotest.test_case "int64 boundary" `Quick test_int_boundary;
          Alcotest.test_case "unknown tag" `Quick test_rejects_unknown_tag ] )
    ]
