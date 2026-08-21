open Ton_cell

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let unhex s =
  String.init (String.length s / 2) (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))

let sha256 s = hex Digestif.SHA256.(to_raw_string (digest_string s))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let expected =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/builder-expected.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "builder-expected.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_int = function `Int n -> n | _ -> Alcotest.fail "expected an int"
let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"
let to_list = function `List l -> l | _ -> Alcotest.fail "expected a list"

(* Cases are stored as little programs of store operations, interpreted
   identically here and by tools/gen-vectors. Both implementations run the
   same spec rather than two hand transcriptions of it. *)
let rec build ops =
  let b =
    List.fold_left
      (fun b op ->
        match op with
        | `List [ `String "uint"; `String v; `Int bits ] -> Builder.store_uint_z b (Z.of_string v) ~bits
        | `List [ `String "int"; `String v; `Int bits ] -> Builder.store_int_z b (Z.of_string v) ~bits
        | `List [ `String "bit"; `Bool x ] -> Builder.store_bit b x
        | `List [ `String "bytes"; `String h ] -> Builder.store_bytes b (unhex h)
        | `List [ `String "ref"; `List inner ] -> Builder.store_ref b (build inner)
        | `List [ `String "maybe_ref"; `Null ] -> Builder.store_maybe_ref b None
        | `List [ `String "maybe_ref"; `List inner ] -> Builder.store_maybe_ref b (Some (build inner))
        | _ -> Alcotest.failf "unrecognised builder op")
      (Builder.create ()) ops
  in
  match Builder.end_cell b with
  | Ok c -> c
  | Error e -> Alcotest.failf "end_cell: %a" Builder.pp_error e

(* Reading the same program back must recover the same values, which is what
   makes Builder and Slice inverses rather than merely compatible. *)
let rec verify_read s ops =
  List.iter
    (fun op ->
      match op with
      | `List [ `String "uint"; `String v; `Int bits ] ->
          Alcotest.(check string)
            (Printf.sprintf "read uint %d bits" bits)
            v
            (Z.to_string (Slice.load_uint_z s ~bits))
      | `List [ `String "int"; `String v; `Int bits ] ->
          Alcotest.(check string)
            (Printf.sprintf "read int %d bits" bits)
            v
            (Z.to_string (Slice.load_int_z s ~bits))
      | `List [ `String "bit"; `Bool x ] -> Alcotest.(check bool) "read bit" x (Slice.load_bit s)
      | `List [ `String "bytes"; `String h ] ->
          Alcotest.(check string) "read bytes" h (hex (Slice.load_bytes s (String.length h / 2)))
      | `List [ `String "ref"; `List inner ] -> verify_read (Slice.of_cell (Slice.load_ref s)) inner
      | `List [ `String "maybe_ref"; `Null ] ->
          Alcotest.(check bool) "maybe_ref absent" true (Slice.load_maybe_ref s = None)
      | `List [ `String "maybe_ref"; `List inner ] -> (
          match Slice.load_maybe_ref s with
          | None -> Alcotest.fail "maybe_ref: expected a reference"
          | Some c -> verify_read (Slice.of_cell c) inner)
      | _ -> Alcotest.failf "unrecognised builder op")
    ops

let case name () =
  let spec = to_obj (field name (Lazy.force expected)) in
  let ops = to_list (field "ops" spec) in
  let c = build ops in
  let at f = Printf.sprintf "%s: %s" name f in
  Alcotest.(check int) (at "bits") (to_int (field "bits" spec)) (Bits.length (Cell.bits c));
  Alcotest.(check int) (at "refs") (to_int (field "refs" spec)) (Cell.ref_count c);
  Alcotest.(check string) (at "hash") (to_str (field "hash0" spec)) (hex (Cell.hash c));
  Alcotest.(check int) (at "depth") (to_int (field "depth0" spec)) (Cell.depth c);
  let boc = Boc.serialize c in
  Alcotest.(check int) (at "boc length") (to_int (field "boc_len" spec)) (String.length boc);
  Alcotest.(check string) (at "boc bytes") (to_str (field "boc_sha256" spec)) (sha256 boc);
  match Slice.parse c (fun s -> verify_read s ops; Slice.end_parse s) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: reading back failed: %a" name Slice.pp_error e

let case_names = lazy (List.map fst (Lazy.force expected))

(* --- overflow ------------------------------------------------------------- *)

(* Writes never fail individually; the first overflow is latched and reported
   by end_cell. Check the latch works and that later writes are dropped rather
   than corrupting the buffer. *)
let test_bit_overflow () =
  let b = Builder.store_bytes (Builder.create ()) (String.make 127 '\xff') in
  Alcotest.(check int) "1016 bits so far" 1016 (Builder.bit_length b);
  Alcotest.(check int) "7 available" 7 (Builder.available_bits b);
  match Builder.end_cell (Builder.store_uint b 0xffL ~bits:8) with
  | Ok _ -> Alcotest.fail "expected an overflow"
  | Error e ->
      Alcotest.(check string) "error" "cannot store 8 bits: only 7 of 1023 remain"
        (Format.asprintf "%a" Builder.pp_error e)

let test_ref_overflow () =
  let b = List.fold_left (fun b _ -> Builder.store_ref b Cell.empty) (Builder.create ()) [ 1; 2; 3; 4 ] in
  Alcotest.(check int) "four refs" 4 (Builder.ref_count b);
  match Builder.end_cell (Builder.store_ref b Cell.empty) with
  | Ok _ -> Alcotest.fail "expected an overflow"
  | Error e ->
      Alcotest.(check string) "error" "cannot store a reference: 4 of 4 already stored"
        (Format.asprintf "%a" Builder.pp_error e)

let test_first_error_wins () =
  let b = Builder.store_uint (Builder.create ()) 0L ~bits:99 in
  let b = List.fold_left (fun b _ -> Builder.store_ref b Cell.empty) b [ 1; 2; 3; 4; 5 ] in
  match Builder.end_cell b with
  | Ok _ -> Alcotest.fail "expected an error"
  | Error e ->
      Alcotest.(check string) "first error is kept" "invalid width 99"
        (Format.asprintf "%a" Builder.pp_error e)

(* --- slice errors --------------------------------------------------------- *)

let test_slice_underrun () =
  let c = build [ `List [ `String "uint"; `String "1"; `Int 8 ] ] in
  match Slice.parse c (fun s -> Slice.load_uint s ~bits:16) with
  | Ok _ -> Alcotest.fail "expected an underrun"
  | Error e ->
      Alcotest.(check string) "error" "need 16 more bits, 8 remain" (Format.asprintf "%a" Slice.pp_error e)

let test_slice_no_ref () =
  match Slice.parse Cell.empty (fun s -> Slice.load_ref s) with
  | Ok _ -> Alcotest.fail "expected a missing reference"
  | Error e ->
      Alcotest.(check string) "error" "need another reference, 0 remain"
        (Format.asprintf "%a" Slice.pp_error e)

let test_end_parse () =
  let c = build [ `List [ `String "uint"; `String "1"; `Int 8 ] ] in
  match Slice.parse c (fun s -> Slice.end_parse s) with
  | Ok () -> Alcotest.fail "expected trailing bits"
  | Error e ->
      Alcotest.(check string) "error" "8 bits left unparsed" (Format.asprintf "%a" Slice.pp_error e)

let test_copy_is_independent () =
  let c = build [ `List [ `String "uint"; `String "43981"; `Int 16 ] ] in
  Result.get_ok
    (Slice.parse c (fun s ->
         let look = Slice.copy s in
         Alcotest.(check int64) "lookahead reads" 0xabcdL (Slice.load_uint look ~bits:16);
         Alcotest.(check int) "original untouched" 16 (Slice.remaining_bits s);
         Alcotest.(check int64) "original still reads" 0xabcdL (Slice.load_uint s ~bits:16)))

(* --- properties ----------------------------------------------------------- *)

let prop_uint_roundtrip =
  QCheck2.Test.make ~count:2000 ~name:"store_uint / load_uint roundtrip"
    QCheck2.Gen.(pair (int_range 1 64) int64)
    (fun (bits, v) ->
      let v = if bits = 64 then v else Int64.logand v (Int64.sub (Int64.shift_left 1L bits) 1L) in
      let c = Result.get_ok (Builder.end_cell (Builder.store_uint (Builder.create ()) v ~bits)) in
      Slice.parse c (fun s -> Slice.load_uint s ~bits) = Ok v)

let prop_int_roundtrip =
  QCheck2.Test.make ~count:2000 ~name:"store_int_z / load_int_z roundtrip"
    QCheck2.Gen.(pair (int_range 1 257) (int_range (-1000000) 1000000))
    (fun (bits, v) ->
      let v = Z.of_int v in
      let lo = Z.neg (Z.shift_left Z.one (bits - 1)) and hi = Z.sub (Z.shift_left Z.one (bits - 1)) Z.one in
      Z.lt v lo || Z.gt v hi
      ||
      let c = Result.get_ok (Builder.end_cell (Builder.store_int_z (Builder.create ()) v ~bits)) in
      Slice.parse c (fun s -> Slice.load_int_z s ~bits) = Ok v)

let prop_bits_roundtrip =
  QCheck2.Test.make ~count:1000 ~name:"store_bits / load_bits roundtrip"
    QCheck2.Gen.(
      let* len = int_range 0 1023 in
      let+ b = string_size ~gen:char (return ((len + 7) / 8)) in
      Bits.sub (Bits.of_bytes b) 0 len)
    (fun b ->
      let c = Result.get_ok (Builder.end_cell (Builder.store_bits (Builder.create ()) b)) in
      match Slice.parse c (fun s -> Slice.load_bits s (Bits.length b)) with
      | Ok r -> Bits.equal b r
      | Error _ -> false)

let () =
  Alcotest.run "builder"
    [ ("vs reference", List.map (fun n -> Alcotest.test_case n `Quick (case n)) (Lazy.force case_names));
      ( "overflow",
        [ Alcotest.test_case "bit overflow" `Quick test_bit_overflow;
          Alcotest.test_case "ref overflow" `Quick test_ref_overflow;
          Alcotest.test_case "first error wins" `Quick test_first_error_wins ] );
      ( "slice",
        [ Alcotest.test_case "underrun" `Quick test_slice_underrun;
          Alcotest.test_case "missing reference" `Quick test_slice_no_ref;
          Alcotest.test_case "end_parse" `Quick test_end_parse;
          Alcotest.test_case "copy is independent" `Quick test_copy_is_independent ] );
      ( "properties",
        List.map
          (QCheck_alcotest.to_alcotest ~verbose:false)
          [ prop_uint_roundtrip; prop_int_roundtrip; prop_bits_roundtrip ] )
    ]
