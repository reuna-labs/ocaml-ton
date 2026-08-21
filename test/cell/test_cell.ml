open Ton_cell

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let ok = function
  | Ok c -> c
  | Error e -> Alcotest.failf "unexpected cell error: %a" Cell.pp_error e

let cell ?(exotic = false) ?(refs = []) bits = ok (Cell.make ~exotic bits refs)
let check_hash name expected c = Alcotest.(check string) name expected (hex (Cell.hash c))

(* --- known hashes --------------------------------------------------------- *)

(* The empty cell is sha256 of its two zero descriptor bytes, and is the most
   widely published cell hash in TON. If this breaks, everything is broken. *)
let test_empty () =
  check_hash "empty cell" "96a296d224f285c67bee93c30f8a309157f0daa35dc5b87e410b78630a09cfc7" Cell.empty;
  Alcotest.(check int) "empty depth" 0 (Cell.depth Cell.empty);
  Alcotest.(check int) "empty level" 0 (Cell.level Cell.empty);
  Alcotest.(check int) "empty refs" 0 (Cell.ref_count Cell.empty)

(* Hand-computed from the spec: d1 = 1 ref, d2 = 0 bits, then the child's
   depth (0) as a big-endian uint16, then the child's 32-byte hash. *)
let test_one_ref () =
  let c = cell ~refs:[ Cell.empty ] Bits.empty in
  let expected =
    hex
      (Digestif.SHA256.to_raw_string
         (Digestif.SHA256.digest_string
            ("\x01\x00" ^ "\x00\x00" ^ Cell.hash Cell.empty)))
  in
  check_hash "cell with one ref" expected c;
  Alcotest.(check int) "depth is 1" 1 (Cell.depth c)

let test_depth () =
  let rec chain n acc = if n = 0 then acc else chain (n - 1) (cell ~refs:[ acc ] Bits.empty) in
  Alcotest.(check int) "depth of a 10-deep chain" 10 (Cell.depth (chain 10 Cell.empty));
  (* Four refs to the same child: depth is max+1, not a sum. *)
  let wide = cell ~refs:(List.init 4 (fun _ -> chain 3 Cell.empty)) Bits.empty in
  Alcotest.(check int) "depth is max+1" 4 (Cell.depth wide)

(* Distinct data must give distinct hashes, and the completion tag must make
   short-but-different bit strings distinguishable. *)
let test_distinct () =
  let a = cell (Bits.of_bytes "\x00") in
  let b = cell (Bits.sub (Bits.of_bytes "\x00") 0 1) in
  Alcotest.(check bool) "1 bit differs from 8 bits" false (String.equal (Cell.hash a) (Cell.hash b));
  Alcotest.(check bool) "empty differs from 1 zero bit" false
    (String.equal (Cell.hash Cell.empty) (Cell.hash b))

(* --- limits --------------------------------------------------------------- *)

let err_case name bits refs expected =
  Alcotest.test_case name `Quick (fun () ->
      match Cell.make ~exotic:false bits refs with
      | Ok _ -> Alcotest.failf "expected rejection"
      | Error e -> Alcotest.(check string) "error" expected (Format.asprintf "%a" Cell.pp_error e))

let big n = Bits.sub (Bits.of_bytes (String.make 200 '\xff')) 0 n

let test_limits =
  [ err_case "1024 bits rejected" (big 1024) [] "cell has 1024 bits, maximum is 1023";
    err_case "5 refs rejected" Bits.empty (List.init 5 (fun _ -> Cell.empty))
      "cell has 5 refs, maximum is 4";
    Alcotest.test_case "1023 bits accepted" `Quick (fun () ->
        Alcotest.(check int) "bits" 1023 (Bits.length (Cell.bits (cell (big 1023)))));
    Alcotest.test_case "4 refs accepted" `Quick (fun () ->
        Alcotest.(check int) "refs" 4
          (Cell.ref_count (cell ~refs:(List.init 4 (fun _ -> Cell.empty)) Bits.empty))) ]

(* --- exotic cells --------------------------------------------------------- *)

let exotic_err name bits refs expected =
  Alcotest.test_case name `Quick (fun () ->
      match Cell.make ~exotic:true bits refs with
      | Ok _ -> Alcotest.failf "expected rejection"
      | Error e -> Alcotest.(check string) "error" expected (Format.asprintf "%a" Cell.pp_error e))

(* A Merkle proof cell wraps a child and restates its level-0 hash and depth;
   construction must verify that restatement rather than trust it. *)
let merkle_proof_bits child =
  Bits.concat
    [ Bits.sub (Bits.of_bytes "\x03") 0 8;
      Bits.of_bytes (Cell.hash child);
      Bits.of_bytes
        (let d = Cell.depth child in
         String.init 2 (fun i -> Char.chr (if i = 0 then d lsr 8 else d land 0xff))) ]

let test_merkle_proof () =
  let child = cell ~refs:[ Cell.empty ] (Bits.of_bytes "\xde\xad\xbe\xef") in
  let p = ok (Cell.make ~exotic:true (merkle_proof_bits child) [ child ]) in
  Alcotest.(check bool) "is exotic" true (Cell.is_exotic p);
  Alcotest.check
    (Alcotest.testable Cell_type.pp Cell_type.equal)
    "type" Cell_type.Merkle_proof (Cell.cell_type p);
  (* A proof's own depth is its child's depth + 1. *)
  Alcotest.(check int) "proof depth" (Cell.depth child + 1) (Cell.depth p)

(* Substituting a different cell of the *same depth* isolates the hash check;
   otherwise the depth check fires first and the hash path goes untested. *)
let test_merkle_proof_tampered_hash () =
  let child = cell ~refs:[ Cell.empty ] (Bits.of_bytes "\xde\xad\xbe\xef") in
  let other = cell ~refs:[ Cell.empty ] (Bits.of_bytes "\x01") in
  Alcotest.(check int) "same depth, so the hash check is what fires" (Cell.depth child)
    (Cell.depth other);
  match Cell.make ~exotic:true (merkle_proof_bits other) [ child ] with
  | Ok _ -> Alcotest.fail "tampered merkle proof was accepted"
  | Error e ->
      Alcotest.(check string) "error" "merkle cell stored hash 0 does not match its ref"
        (Format.asprintf "%a" Cell.pp_error e)

let test_merkle_proof_tampered_depth () =
  let child = cell ~refs:[ Cell.empty ] (Bits.of_bytes "\xde\xad\xbe\xef") in
  (* Correct hash, wrong depth. *)
  let bits =
    Bits.concat
      [ Bits.sub (Bits.of_bytes "\x03") 0 8; Bits.of_bytes (Cell.hash child);
        Bits.of_bytes "\x00\x07" ]
  in
  match Cell.make ~exotic:true bits [ child ] with
  | Ok _ -> Alcotest.fail "merkle proof with wrong depth was accepted"
  | Error e ->
      Alcotest.(check string) "error" "merkle cell stored depth 0 does not match its ref"
        (Format.asprintf "%a" Cell.pp_error e)

(* A pruned branch stands in for an elided subtree: it stores the hash and
   depth it replaces, and reports them as its own at the lower level. *)
let test_pruned_branch () =
  let elided = cell ~refs:[ Cell.empty ] (Bits.of_bytes "\xca\xfe") in
  let bits =
    Bits.concat
      [ Bits.of_bytes "\x01" (* type *); Bits.of_bytes "\x01" (* mask: level 1 *);
        Bits.of_bytes (Cell.hash elided);
        Bits.of_bytes (String.init 2 (fun i ->
            let d = Cell.depth elided in
            Char.chr (if i = 0 then d lsr 8 else d land 0xff))) ]
  in
  let p = ok (Cell.make ~exotic:true bits []) in
  Alcotest.(check int) "pruned bit length" 288 (Bits.length (Cell.bits p));
  Alcotest.(check int) "level" 1 (Cell.level p);
  (* At level 0 it impersonates the cell it replaced -- this is exactly what
     makes a Merkle proof check out against the unpruned tree. *)
  Alcotest.(check string) "level-0 hash is the elided cell's" (hex (Cell.hash elided))
    (hex (Cell.hash ~level:0 p));
  Alcotest.(check int) "level-0 depth is the elided cell's" (Cell.depth elided)
    (Cell.depth ~level:0 p);
  Alcotest.(check bool) "level-1 hash is its own" false
    (String.equal (Cell.hash ~level:1 p) (Cell.hash elided))

let test_exotic_errors =
  [ exotic_err "unknown type" (Bits.of_bytes "\x09") [] "unknown exotic cell type 9";
    exotic_err "too short" (Bits.sub (Bits.of_bytes "\x03") 0 4) []
      "exotic cell has 4 bits, need at least 8 for the type tag";
    exotic_err "library wrong size" (Bits.of_bytes "\x02") []
      "library cell must have exactly 264 bits, got 8";
    exotic_err "merkle proof wrong refs"
      (Bits.concat [ Bits.of_bytes "\x03"; Bits.of_bytes (String.make 34 '\x00') ])
      [] "merkle proof must have exactly 1 ref, got 0";
    exotic_err "pruned with refs"
      (Bits.concat [ Bits.of_bytes "\x01\x01"; Bits.of_bytes (String.make 34 '\x00') ])
      [ Cell.empty ] "pruned branch must have no refs, got 1" ]

let () =
  Alcotest.run "cell"
    [ ( "hashing",
        [ Alcotest.test_case "empty cell" `Quick test_empty;
          Alcotest.test_case "one ref" `Quick test_one_ref;
          Alcotest.test_case "depth" `Quick test_depth;
          Alcotest.test_case "distinct data" `Quick test_distinct ] );
      ("limits", test_limits);
      ( "exotic",
        [ Alcotest.test_case "merkle proof" `Quick test_merkle_proof;
          Alcotest.test_case "merkle proof tampered hash" `Quick test_merkle_proof_tampered_hash;
          Alcotest.test_case "merkle proof tampered depth" `Quick test_merkle_proof_tampered_depth;
          Alcotest.test_case "pruned branch" `Quick test_pruned_branch ] );
      ("exotic errors", test_exotic_errors)
    ]
