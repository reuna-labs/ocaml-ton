open Ton_cell
open Ton_tlb

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let sha256 s = hex Digestif.SHA256.(to_raw_string (digest_string s))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let expected =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/dict-expected.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "dict-expected.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_int = function `Int n -> n | _ -> Alcotest.fail "expected an int"
let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"
let to_list = function `List l -> l | _ -> Alcotest.fail "expected a list"

let ok what = function Ok v -> v | Error e -> Alcotest.failf "%s: %a" what Dict.pp_error e

let load_case name =
  let spec = to_obj (field name (Lazy.force expected)) in
  let key_bits = to_int (field "keyBits" spec) and value_bits = to_int (field "valueBits" spec) in
  let entries =
    List.map
      (function
        | `List [ `String k; `String v ] -> (Z.of_string k, Z.of_string v)
        | _ -> Alcotest.fail "malformed dictionary entry")
      (to_list (field "entries" spec))
  in
  (spec, key_bits, value_bits, entries)

let case name () =
  let spec, key_bits, value_bits, entries = load_case name in
  let d = Dict.of_list ~key_bits entries in
  let store b v = Builder.store_uint_z b v ~bits:value_bits in
  let read s = Slice.load_uint_z s ~bits:value_bits in
  let at f = Printf.sprintf "%s: %s" name f in
  Alcotest.(check int) (at "cardinal") (List.length entries) (Dict.cardinal d);

  (* Hashmap, written straight into a cell. *)
  (match List.assoc_opt "direct" spec with
  | None -> ()
  | Some direct ->
      let direct = to_obj direct in
      let c = ok name (Dict.to_cell ~value:store d) in
      Alcotest.(check string) (at "direct hash") (to_str (field "hash" direct)) (hex (Cell.hash c));
      Alcotest.(check int) (at "direct bits") (to_int (field "bits" direct)) (Bits.length (Cell.bits c));
      Alcotest.(check int) (at "direct refs") (to_int (field "refs" direct)) (Cell.ref_count c);
      (* And it reads back to the same entries. *)
      let back =
        match Dict.of_cell c ~key_bits ~value:read with
        | Ok d -> d
        | Error e -> Alcotest.failf "%s: reparse: %a" name Slice.pp_error e
      in
      Alcotest.(check bool) (at "not partial") false (Dict.is_partial back);
      (* to_list is ascending by key; the vectors list entries in insertion
         order, so sort before comparing. *)
      let sorted l =
        List.map (fun (k, v) -> (Z.to_string k, Z.to_string v))
          (List.sort (fun (a, _) (b, _) -> Z.compare a b) l)
      in
      Alcotest.(check (list (pair string string)))
        (at "entries survive") (sorted entries) (sorted (Dict.to_list back)));

  (* HashmapE, which is the form that actually appears inside other structures. *)
  let wrapped = to_obj (field "wrapped" spec) in
  let wc =
    match Builder.end_cell (ok name (Dict.store_maybe (Builder.create ()) ~value:store d)) with
    | Ok c -> c
    | Error e -> Alcotest.failf "%s: %a" name Builder.pp_error e
  in
  Alcotest.(check string) (at "wrapped hash") (to_str (field "hash" wrapped)) (hex (Cell.hash wc));
  Alcotest.(check string) (at "wrapped boc") (to_str (field "boc_sha256" wrapped)) (sha256 (Boc.serialize wc));
  match Slice.parse wc (fun s -> Dict.load_maybe s ~key_bits ~value:read) with
  | Error e -> Alcotest.failf "%s: reparse wrapped: %a" name Slice.pp_error e
  | Ok back ->
      Alcotest.(check int) (at "wrapped cardinal") (List.length entries) (Dict.cardinal back)

let case_names = lazy (List.map fst (Lazy.force expected))

(* --- behaviour ------------------------------------------------------------ *)

let d8 l = Dict.of_list ~key_bits:8 (List.map (fun (k, v) -> (Z.of_int k, Z.of_int v)) l)
let store8 b v = Builder.store_uint_z b v ~bits:8
let read8 s = Slice.load_uint_z s ~bits:8

let test_lookup () =
  let d = d8 [ (1, 10); (2, 20); (255, 30) ] in
  Alcotest.(check (option string)) "found" (Some "20") (Option.map Z.to_string (Dict.find (Z.of_int 2) d));
  Alcotest.(check (option string)) "absent" None (Option.map Z.to_string (Dict.find (Z.of_int 3) d));
  Alcotest.(check bool) "mem" true (Dict.mem (Z.of_int 255) d);
  Alcotest.(check int) "cardinal" 3 (Dict.cardinal d);
  let d = Dict.remove (Z.of_int 2) d in
  Alcotest.(check int) "after remove" 2 (Dict.cardinal d);
  let d = Dict.add (Z.of_int 7) (Z.of_int 70) d in
  Alcotest.(check (option string)) "after add" (Some "70") (Option.map Z.to_string (Dict.find (Z.of_int 7) d))

let test_ordering () =
  let d = d8 [ (255, 1); (0, 2); (128, 3); (1, 4) ] in
  Alcotest.(check (list string)) "ascending by key" [ "0"; "1"; "128"; "255" ]
    (List.map (fun (k, _) -> Z.to_string k) (Dict.to_list d))

(* Insertion order must not affect the encoding, or two peers would compute
   different hashes for the same map. *)
let test_insertion_order_irrelevant () =
  let a = d8 [ (1, 1); (2, 2); (3, 3); (200, 4) ] in
  let b = d8 [ (200, 4); (3, 3); (1, 1); (2, 2) ] in
  let ca = ok "a" (Dict.to_cell ~value:store8 a) and cb = ok "b" (Dict.to_cell ~value:store8 b) in
  Alcotest.(check string) "same hash" (hex (Cell.hash ca)) (hex (Cell.hash cb))

let test_empty () =
  let e = Dict.empty ~key_bits:8 in
  Alcotest.(check bool) "is empty" true (Dict.is_empty e);
  (* Hashmap has no empty encoding; HashmapE does. *)
  (match Dict.to_cell ~value:store8 e with
  | Ok _ -> Alcotest.fail "empty dictionary should have no Hashmap encoding"
  | Error err ->
      Alcotest.(check string) "error"
        "an empty dictionary has no Hashmap encoding; use store_maybe"
        (Format.asprintf "%a" Dict.pp_error err));
  let c =
    Result.get_ok (Builder.end_cell (ok "e" (Dict.store_maybe (Builder.create ()) ~value:store8 e)))
  in
  Alcotest.(check int) "one bit" 1 (Bits.length (Cell.bits c));
  Alcotest.(check int) "no refs" 0 (Cell.ref_count c);
  match Slice.parse c (fun s -> Dict.load_maybe s ~key_bits:8 ~value:read8) with
  | Ok back -> Alcotest.(check bool) "reads back empty" true (Dict.is_empty back)
  | Error e -> Alcotest.failf "%a" Slice.pp_error e

let test_key_out_of_range () =
  let d = Dict.of_list ~key_bits:4 [ (Z.of_int 255, Z.one) ] in
  match Dict.to_cell ~value:store8 d with
  | Ok _ -> Alcotest.fail "accepted a key wider than the key space"
  | Error e ->
      Alcotest.(check string) "error" "key 255 does not fit the dictionary's key width"
        (Format.asprintf "%a" Dict.pp_error e)

(* A dictionary inside a Merkle proof has subtrees replaced by pruned
   branches. Those keys are missing from the result, and the caller has to be
   able to tell that apart from genuine absence. *)
let test_partial_from_pruned () =
  let d = d8 [ (0, 1); (255, 2) ] in
  let c = ok "d" (Dict.to_cell ~value:store8 d) in
  Alcotest.(check int) "the root forks" 2 (Cell.ref_count c);
  let left = Option.get (Cell.nth_ref c 0) in
  (* Replace the left branch with a pruned branch standing for it. *)
  let pruned =
    let b = Builder.create () in
    let b = Builder.store_uint b 1L ~bits:8 (* type: pruned branch *) in
    let b = Builder.store_uint b 1L ~bits:8 (* mask: level 1 *) in
    let b = Builder.store_bytes b (Cell.hash left) in
    let b = Builder.store_uint b (Int64.of_int (Cell.depth left)) ~bits:16 in
    Result.get_ok (Builder.end_cell ~exotic:true b)
  in
  let right = Option.get (Cell.nth_ref c 1) in
  let proofed =
    Result.get_ok
      (Cell.make ~exotic:false (Cell.bits c) [ pruned; right ])
  in
  match Dict.of_cell proofed ~key_bits:8 ~value:read8 with
  | Error e -> Alcotest.failf "%a" Slice.pp_error e
  | Ok back ->
      Alcotest.(check bool) "flagged as partial" true (Dict.is_partial back);
      Alcotest.(check int) "only the unpruned side survives" 1 (Dict.cardinal back);
      Alcotest.(check bool) "the pruned key is absent" false (Dict.mem Z.zero back);
      Alcotest.(check bool) "the visible key is present" true (Dict.mem (Z.of_int 255) back);
      (* The whole point: the pruned tree still hashes to the original. *)
      Alcotest.(check string) "hash is preserved" (hex (Cell.hash c)) (hex (Cell.hash proofed))

(* --- properties ----------------------------------------------------------- *)

let prop_roundtrip =
  QCheck2.Test.make ~count:400 ~name:"dictionary roundtrip for random key widths"
    QCheck2.Gen.(
      let* key_bits = int_range 1 64 in
      let+ keys = list_size (int_range 1 24) (int_range 0 max_int) in
      (key_bits, keys))
    (fun (key_bits, keys) ->
      let mask k = Z.logand (Z.of_int (abs k)) (Z.sub (Z.shift_left Z.one key_bits) Z.one) in
      let entries =
        List.sort_uniq (fun (a, _) (b, _) -> Z.compare a b)
          (List.mapi (fun i k -> (mask k, Z.of_int (i mod 256))) keys)
      in
      let d = Dict.of_list ~key_bits entries in
      match Dict.to_cell ~value:store8 d with
      | Error _ -> false
      | Ok c -> (
          match Dict.of_cell c ~key_bits ~value:read8 with
          | Error _ -> false
          | Ok back -> Dict.to_list back = Dict.to_list d))

let () =
  Alcotest.run "dict"
    [ ("vs reference", List.map (fun n -> Alcotest.test_case n `Quick (case n)) (Lazy.force case_names));
      ( "behaviour",
        [ Alcotest.test_case "lookup" `Quick test_lookup;
          Alcotest.test_case "ordering" `Quick test_ordering;
          Alcotest.test_case "insertion order is irrelevant" `Quick test_insertion_order_irrelevant;
          Alcotest.test_case "empty" `Quick test_empty;
          Alcotest.test_case "key out of range" `Quick test_key_out_of_range;
          Alcotest.test_case "pruned branches make it partial" `Quick test_partial_from_pruned ] );
      ("properties", [ QCheck_alcotest.to_alcotest ~verbose:false prop_roundtrip ])
    ]
