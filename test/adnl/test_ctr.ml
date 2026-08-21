open Ton_adnl

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let unhex s =
  String.init (String.length s / 2) (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))

(* NIST SP 800-38A, F.5.5: CTR-AES256.Encrypt. This pins the counter's own
   arithmetic -- whether it increments as a 128-bit big-endian value -- which
   ADNL depends on and which is not obvious from the interface. *)
let test_nist () =
  let key = unhex "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4" in
  let iv = unhex "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff" in
  let plain =
    unhex
      ("6bc1bee22e409f96e93d7e117393172a" ^ "ae2d8a571e03ac9c9eb76fac45af8e51"
     ^ "30c81c46a35ce411e5fbc1191a0a52ef" ^ "f69f2445df4f9b17ad2b417be66c3710")
  in
  let want =
    "601ec313775789a5b7a7f504bbf3d228" ^ "f443e3ca4d62b59aca84e990cacaf5c5"
    ^ "2b0930daa23de94ce87017ba2d84988d" ^ "dfc9c58db67aada613c2dd08457941a6"
  in
  let _, out = Ctr.xor (Ctr.create ~key ~iv) plain in
  Alcotest.(check string) "four blocks" want (hex out);
  (* And decryption is the same operation. *)
  let _, back = Ctr.xor (Ctr.create ~key ~iv) out in
  Alcotest.(check string) "roundtrip" (hex plain) (hex back)

(* The stream must not care how the input is chopped up. This is the property
   that actually protects ADNL, whose frames are arbitrary lengths and whose
   length prefix is itself part of the same stream. *)
let test_chunking () =
  let key = String.make 32 '\x2b' and iv = String.make 16 '\x7e' in
  let input = String.init 300 (fun i -> Char.chr (i * 37 land 0xff)) in
  let _, whole = Ctr.xor (Ctr.create ~key ~iv) input in
  for split = 0 to String.length input do
    let t = Ctr.create ~key ~iv in
    let t, a = Ctr.xor t (String.sub input 0 split) in
    let _, b = Ctr.xor t (String.sub input split (String.length input - split)) in
    Alcotest.(check string) (Printf.sprintf "split at %d" split) (hex whole) (hex (a ^ b))
  done

let test_many_chunks () =
  let key = String.make 32 '\x01' and iv = String.make 16 '\x02' in
  let input = String.init 1000 (fun i -> Char.chr (i land 0xff)) in
  let _, whole = Ctr.xor (Ctr.create ~key ~iv) input in
  (* One byte at a time is the worst case for partial-block carry-over. *)
  let b = Buffer.create 1000 in
  let t = ref (Ctr.create ~key ~iv) in
  String.iter
    (fun c ->
      let t', o = Ctr.xor !t (String.make 1 c) in
      t := t';
      Buffer.add_string b o)
    input;
  Alcotest.(check string) "byte at a time" (hex whole) (hex (Buffer.contents b));
  Alcotest.(check int) "offset tracks" 1000 (Ctr.offset !t)

(* The counter is 128 bits and has to carry, not wrap at 64. Starting one
   block before the low half rolls over exercises that. *)
let test_counter_carry () =
  let key = String.make 32 '\x03' in
  let iv = unhex "00000000000000ffffffffffffffffff" in
  let input = String.make 64 '\x00' in
  let _, whole = Ctr.xor (Ctr.create ~key ~iv) input in
  let t = Ctr.create ~key ~iv in
  let t, a = Ctr.xor t (String.sub input 0 16) in
  let _, rest = Ctr.xor t (String.sub input 16 48) in
  Alcotest.(check string) "keystream is continuous across the carry" (hex whole) (hex (a ^ rest))

let test_argument_checks () =
  Alcotest.check_raises "short key" (Invalid_argument "Ctr.create: key must be 32 bytes") (fun () ->
      ignore (Ctr.create ~key:"short" ~iv:(String.make 16 '\x00')));
  Alcotest.check_raises "short iv" (Invalid_argument "Ctr.create: iv must be 16 bytes") (fun () ->
      ignore (Ctr.create ~key:(String.make 32 '\x00') ~iv:"short"))

let prop_chunking =
  QCheck2.Test.make ~count:500 ~name:"arbitrary chopping gives the same stream"
    QCheck2.Gen.(pair (string_size (int_range 0 400)) (list_size (int_range 0 12) (int_range 0 60)))
    (fun (input, splits) ->
      let key = String.make 32 '\x11' and iv = String.make 16 '\x22' in
      let _, whole = Ctr.xor (Ctr.create ~key ~iv) input in
      let b = Buffer.create (String.length input) in
      let t = ref (Ctr.create ~key ~iv) in
      let pos = ref 0 in
      List.iter
        (fun k ->
          let take = min k (String.length input - !pos) in
          let t', o = Ctr.xor !t (String.sub input !pos take) in
          t := t';
          Buffer.add_string b o;
          pos := !pos + take)
        splits;
      let _, o = Ctr.xor !t (String.sub input !pos (String.length input - !pos)) in
      Buffer.add_string b o;
      String.equal whole (Buffer.contents b))

let () =
  Alcotest.run "adnl ctr"
    [ ( "keystream",
        [ Alcotest.test_case "NIST SP 800-38A AES-256-CTR" `Quick test_nist;
          Alcotest.test_case "chunk boundaries" `Quick test_chunking;
          Alcotest.test_case "byte at a time" `Quick test_many_chunks;
          Alcotest.test_case "counter carry" `Quick test_counter_carry;
          Alcotest.test_case "argument checks" `Quick test_argument_checks ] );
      ("properties", [ QCheck_alcotest.to_alcotest ~verbose:false prop_chunking ])
    ]
