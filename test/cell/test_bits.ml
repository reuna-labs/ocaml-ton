let bits = Alcotest.testable Ton_cell.Bits.pp Ton_cell.Bits.equal

open Ton_cell

(* --- padding round-trip -------------------------------------------------- *)

(* Padding is not self-describing: a byte-aligned value carries no tag. The BoC
   reader knows which case it is from the parity of the [d2] descriptor, so
   decoding is always parameterised by that bit. This helper is exactly what
   the reader does. *)
let decode ~padded s = if padded then Bits.of_padded_bytes s else Bits.of_bytes s

(* The completion tag is the single most load-bearing rule in the cell format:
   [to_padded_bytes] is what the representation hash is computed over. *)
let test_padding_roundtrip () =
  for len = 0 to 1023 do
    let b = Bits.sub (Bits.of_bytes (String.init 128 (fun i -> Char.chr (i * 7 land 0xff)))) 0 len in
    let s = Bits.to_padded_bytes b in
    Alcotest.(check int)
      (Printf.sprintf "padded length for %d bits" len)
      (if len land 7 = 0 then len / 8 else (len + 7) / 8)
      (String.length s);
    Alcotest.check bits
      (Printf.sprintf "roundtrip %d bits" len)
      b
      (decode ~padded:(len land 7 <> 0) s)
  done

(* The ambiguity itself, pinned so nobody "fixes" it later: an all-zero padded
   buffer decodes to empty, which is why [d2] parity has to be consulted. *)
let test_padding_is_not_self_describing () =
  Alcotest.(check int) "zero byte, read as padded" 0 (Bits.length (Bits.of_padded_bytes "\x00"));
  Alcotest.(check int) "zero byte, read as aligned" 8 (Bits.length (Bits.of_bytes "\x00"))

let test_padding_examples () =
  (* A byte-aligned value gets no tag at all. *)
  Alcotest.(check string) "aligned, no tag" "\xff" (Bits.to_padded_bytes (Bits.of_bytes "\xff"));
  (* 4 bits '1010' -> 1010 1000 : data, tag bit, then zeros. *)
  let b = Bits.sub (Bits.of_bytes "\xa0") 0 4 in
  Alcotest.(check string) "4 bits tagged" "\xa8" (Bits.to_padded_bytes b);
  Alcotest.(check int) "untag 4 bits" 4 (Bits.length (Bits.of_padded_bytes "\xa8"));
  (* All-zero input is the empty bit string, not a run of zero bits. *)
  Alcotest.(check int) "all zero is empty" 0 (Bits.length (Bits.of_padded_bytes "\x00\x00"))

(* --- access -------------------------------------------------------------- *)

let test_get () =
  let b = Bits.of_bytes "\x80\x01" in
  Alcotest.(check bool) "bit 0 is MSB of byte 0" true (Bits.get b 0);
  Alcotest.(check bool) "bit 1" false (Bits.get b 1);
  Alcotest.(check bool) "bit 15 is LSB of byte 1" true (Bits.get b 15);
  Alcotest.check_raises "out of range" (Invalid_argument "Bits.get: index out of bounds")
    (fun () -> ignore (Bits.get b 16))

let test_uint () =
  let b = Bits.of_bytes "\x12\x34\x56\x78" in
  Alcotest.(check int64) "uint 32" 0x12345678L (Bits.get_uint b ~pos:0 ~len:32);
  Alcotest.(check int64) "uint 8 at 8" 0x34L (Bits.get_uint b ~pos:8 ~len:8);
  Alcotest.(check int64) "uint 4 unaligned" 0x2L (Bits.get_uint b ~pos:4 ~len:4);
  Alcotest.(check int64) "uint 0" 0L (Bits.get_uint b ~pos:0 ~len:0);
  (* 12 bits straddling a byte boundary. *)
  Alcotest.(check int64) "uint 12 at 4" 0x234L (Bits.get_uint b ~pos:4 ~len:12)

let test_int () =
  let b = Bits.of_bytes "\xff\x7f" in
  Alcotest.(check int64) "int8 = -1" (-1L) (Bits.get_int b ~pos:0 ~len:8);
  Alcotest.(check int64) "int8 = 127" 127L (Bits.get_int b ~pos:8 ~len:8);
  Alcotest.(check int64) "int4 = -1" (-1L) (Bits.get_int b ~pos:0 ~len:4);
  Alcotest.(check int64) "int1 = -1" (-1L) (Bits.get_int b ~pos:0 ~len:1);
  Alcotest.(check int64) "int0 = 0" 0L (Bits.get_int b ~pos:0 ~len:0)

(* --- structural ---------------------------------------------------------- *)

let test_concat () =
  let a = Bits.sub (Bits.of_bytes "\xa0") 0 3 (* 101 *) in
  let b = Bits.sub (Bits.of_bytes "\xc0") 0 2 (* 11  *) in
  let c = Bits.concat [ a; b ] in
  Alcotest.(check int) "length" 5 (Bits.length c);
  Alcotest.(check int64) "value 10111" 0b10111L (Bits.get_uint c ~pos:0 ~len:5);
  Alcotest.(check int) "concat [] is empty" 0 (Bits.length (Bits.concat []))

let test_sub () =
  let b = Bits.of_bytes "\x12\x34" in
  Alcotest.check bits "sub of whole" b (Bits.sub b 0 16);
  Alcotest.(check int64) "nested sub" 0x3L (Bits.get_uint (Bits.sub (Bits.sub b 4 8) 4 4) ~pos:0 ~len:4);
  Alcotest.check_raises "past end" (Invalid_argument "Bits.sub: range out of bounds")
    (fun () -> ignore (Bits.sub b 8 9))

(* --- properties ---------------------------------------------------------- *)

let arb_bits =
  let open QCheck2.Gen in
  let* len = int_range 0 1023 in
  let+ bytes = string_size ~gen:char (return ((len + 7) / 8)) in
  Bits.sub (Bits.of_bytes bytes) 0 len

let prop_padding_roundtrip =
  QCheck2.Test.make ~count:2000 ~name:"decode . to_padded_bytes = id" arb_bits (fun b ->
      Bits.equal b (decode ~padded:(Bits.length b land 7 <> 0) (Bits.to_padded_bytes b)))

let prop_concat_length =
  QCheck2.Test.make ~count:1000 ~name:"concat preserves total length"
    (QCheck2.Gen.pair arb_bits arb_bits)
    (fun (a, b) -> Bits.length (Bits.append a b) = Bits.length a + Bits.length b)

let prop_concat_get =
  QCheck2.Test.make ~count:1000 ~name:"concat preserves bits"
    (QCheck2.Gen.pair arb_bits arb_bits)
    (fun (a, b) ->
      let c = Bits.append a b in
      let la = Bits.length a in
      let rec go i =
        i >= Bits.length c
        || (Bits.get c i = (if i < la then Bits.get a i else Bits.get b (i - la)) && go (i + 1))
      in
      go 0)

let prop_sub_identity =
  QCheck2.Test.make ~count:1000 ~name:"sub 0 (length b) = b" arb_bits
    (fun b -> Bits.equal b (Bits.sub b 0 (Bits.length b)))

let () =
  Alcotest.run "bits"
    [ ( "bits",
        [ Alcotest.test_case "padding roundtrip" `Quick test_padding_roundtrip;
          Alcotest.test_case "padding examples" `Quick test_padding_examples;
          Alcotest.test_case "padding is not self-describing" `Quick
            test_padding_is_not_self_describing;
          Alcotest.test_case "get" `Quick test_get;
          Alcotest.test_case "get_uint" `Quick test_uint;
          Alcotest.test_case "get_int" `Quick test_int;
          Alcotest.test_case "concat" `Quick test_concat;
          Alcotest.test_case "sub" `Quick test_sub ] );
      ( "bits properties",
        List.map
          (QCheck_alcotest.to_alcotest ~verbose:false)
          [ prop_padding_roundtrip; prop_concat_length; prop_concat_get; prop_sub_identity ] )
    ]
