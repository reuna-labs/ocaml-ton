open Ton_crypto

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let unhex s =
  String.init (String.length s / 2) (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let vectors =
  lazy
    (match Yojson.Safe.from_string (read_file "../vectors/crypto-expected.json") with
    | `Assoc l -> l
    | _ -> Alcotest.fail "crypto-expected.json: expected an object")

let field name obj =
  match List.assoc_opt name obj with Some v -> v | None -> Alcotest.failf "missing field %S" name

let to_int = function `Int n -> n | _ -> Alcotest.fail "expected an int"
let to_str = function `String s -> s | _ -> Alcotest.fail "expected a string"
let to_bool = function `Bool b -> b | _ -> Alcotest.fail "expected a bool"
let to_obj = function `Assoc l -> l | _ -> Alcotest.fail "expected an object"
let to_list = function `List l -> l | _ -> Alcotest.fail "expected a list"
let section n = field n (Lazy.force vectors)
let strings v = List.map to_str (to_list v)

(* --- wordlist ------------------------------------------------------------- *)

(* We embed our own copy of the BIP-39 list from the Bitcoin BIPs repository.
   Confirm it is the same list the reference uses, or every derivation would
   silently disagree for phrases containing a differing word. *)
let test_wordlist () =
  let spec = to_obj (section "wordlist") in
  Alcotest.(check int) "count" (to_int (field "count" spec)) Mnemonic.word_count;
  Alcotest.(check int) "array length" (to_int (field "count" spec)) (Array.length Mnemonic.words);
  Alcotest.(check string) "first" (to_str (field "first" spec)) Mnemonic.words.(0);
  Alcotest.(check string) "last" (to_str (field "last" spec)) Mnemonic.words.(Array.length Mnemonic.words - 1);
  let joined = String.concat "" (Array.to_list (Array.map (fun w -> w ^ "\n") Mnemonic.words)) in
  Alcotest.(check string) "sha256 of the list" (to_str (field "sha256" spec)) (hex (Hash.sha256 joined));
  Alcotest.(check bool) "membership" true (Mnemonic.is_word "abandon");
  Alcotest.(check bool) "non-membership" false (Mnemonic.is_word "notaword")

(* --- primitives ----------------------------------------------------------- *)

(* Checked against Node's PBKDF2, which shares no code with the TON stack. *)
let test_pbkdf2 () =
  List.iteri
    (fun i spec ->
      let spec = to_obj spec in
      let password = to_str (field "password" spec) in
      let salt = to_str (field "salt" spec) in
      let iterations = to_int (field "iterations" spec) in
      let len = to_int (field "len" spec) in
      Alcotest.(check string)
        (Printf.sprintf "pbkdf2 case %d (c=%d len=%d)" i iterations len)
        (to_str (field "dk" spec))
        (hex (Hash.pbkdf2_sha512 ~password ~salt ~iterations ~len)))
    (to_list (section "pbkdf2"))

let test_hmac () =
  List.iteri
    (fun i spec ->
      let spec = to_obj spec in
      Alcotest.(check string)
        (Printf.sprintf "hmac-sha512 case %d" i)
        (to_str (field "mac" spec))
        (hex (Hash.hmac_sha512 ~key:(unhex (to_str (field "key" spec))) (unhex (to_str (field "data" spec))))))
    (to_list (section "hmac"))

let test_pbkdf2_rejects () =
  Alcotest.check_raises "zero iterations"
    (Invalid_argument "pbkdf2_sha512: iterations must be positive") (fun () ->
      ignore (Hash.pbkdf2_sha512 ~password:"" ~salt:"" ~iterations:0 ~len:64));
  Alcotest.check_raises "zero length"
    (Invalid_argument "pbkdf2_sha512: length must be positive") (fun () ->
      ignore (Hash.pbkdf2_sha512 ~password:"" ~salt:"" ~iterations:1 ~len:0))

(* --- Ed25519 -------------------------------------------------------------- *)

(* RFC 8032 section 7.1, test vector 1. *)
let test_rfc8032 () =
  let kp =
    Result.get_ok (Ed25519.of_seed (unhex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"))
  in
  Alcotest.(check string) "public key" "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
    (hex (Ed25519.public kp));
  Alcotest.(check string) "signature over the empty message"
    "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
    (hex (Ed25519.sign kp ""));
  Alcotest.(check bool) "verifies" true
    (Ed25519.verify ~public:(Ed25519.public kp) ~signature:(Ed25519.sign kp "") "")

let test_ed25519_rejects () =
  (match Ed25519.of_seed "short" with
  | Ok _ -> Alcotest.fail "accepted a short seed"
  | Error e -> Alcotest.(check string) "error" "Ed25519 seed must be 32 bytes, got 5" e);
  let kp = Result.get_ok (Ed25519.of_seed (String.make 32 '\x01')) in
  let sig_ = Ed25519.sign kp "hello" in
  Alcotest.(check bool) "wrong message" false (Ed25519.verify ~public:(Ed25519.public kp) ~signature:sig_ "goodbye");
  Alcotest.(check bool) "truncated signature" false
    (Ed25519.verify ~public:(Ed25519.public kp) ~signature:(String.sub sig_ 0 63) "hello");
  Alcotest.(check bool) "malformed public key" false
    (Ed25519.verify ~public:"nope" ~signature:sig_ "hello");
  Alcotest.(check int) "secret key is the NaCl 64-byte form" 64 (String.length (Ed25519.secret_key kp));
  Alcotest.(check string) "which is seed then public"
    (hex (Ed25519.seed kp) ^ hex (Ed25519.public kp))
    (hex (Ed25519.secret_key kp))

(* --- mnemonics ------------------------------------------------------------ *)

let mnemonic_case i spec () =
  let spec = to_obj spec in
  let words = strings (field "words" spec) in
  let password = match field "password" spec with `Null -> None | v -> Some (to_str v) in
  let at f = Printf.sprintf "mnemonic %d: %s" i f in
  Alcotest.(check int) (at "24 words") 24 (List.length words);
  Alcotest.(check string) (at "entropy") (to_str (field "entropy" spec))
    (hex (Mnemonic.to_entropy ?password words));
  Alcotest.(check string) (at "seed") (to_str (field "seed" spec))
    (hex (Mnemonic.to_seed ?password words));
  Alcotest.(check string) (at "hd seed") (to_str (field "hdSeed" spec))
    (hex (Mnemonic.to_seed ?password ~salt:"TON HD Keys seed" words));
  let kp = Result.get_ok (Mnemonic.to_keypair ?password words) in
  Alcotest.(check string) (at "public key") (to_str (field "publicKey" spec)) (hex (Ed25519.public kp));
  Alcotest.(check string) (at "secret key") (to_str (field "secretKey" spec)) (hex (Ed25519.secret_key kp));
  Alcotest.(check bool) (at "validates") (to_bool (field "valid" spec)) (Mnemonic.validate ?password words);
  (* A signature made with the derived key must verify under the derived
     public key -- the derivation is only useful if the pair is consistent. *)
  let msg = "ocaml-ton" in
  Alcotest.(check bool) (at "key pair is consistent") true
    (Ed25519.verify ~public:(Ed25519.public kp) ~signature:(Ed25519.sign kp msg) msg)

let invalid_case i spec () =
  let spec = to_obj spec in
  let words = strings (field "words" spec) in
  Alcotest.(check bool)
    (Printf.sprintf "invalid %d (%s)" i (to_str (field "reason" spec)))
    (to_bool (field "valid" spec)) (Mnemonic.validate words)

(* A password changes the derivation completely, and the phrase must not
   validate under the wrong one. *)
let test_password_changes_key () =
  let spec = to_obj (List.hd (to_list (section "mnemonics"))) in
  let words = strings (field "words" spec) in
  let a = Result.get_ok (Mnemonic.to_keypair words) in
  let b = Result.get_ok (Mnemonic.to_keypair ~password:"hunter2" words) in
  Alcotest.(check bool) "different keys" false (String.equal (Ed25519.public a) (Ed25519.public b))

let test_normalisation () =
  let spec = to_obj (List.hd (to_list (section "mnemonics"))) in
  let words = strings (field "words" spec) in
  let messy = List.map (fun w -> "  " ^ String.uppercase_ascii w ^ " ") words in
  Alcotest.(check (list string)) "normalise" words (Mnemonic.normalize messy);
  Alcotest.(check string) "same key after normalising"
    (hex (Ed25519.public (Result.get_ok (Mnemonic.to_keypair words))))
    (hex (Ed25519.public (Result.get_ok (Mnemonic.to_keypair messy))));
  Alcotest.(check (list string)) "parse from a string" words
    (Mnemonic.of_string (String.concat "  " words))

let () =
  let mnemonics = to_list (section "mnemonics") in
  let invalid = to_list (section "invalid") in
  Alcotest.run "crypto"
    [ ("wordlist", [ Alcotest.test_case "matches the reference list" `Quick test_wordlist ]);
      ( "primitives",
        [ Alcotest.test_case "pbkdf2-hmac-sha512" `Quick test_pbkdf2;
          Alcotest.test_case "hmac-sha512" `Quick test_hmac;
          Alcotest.test_case "pbkdf2 argument checks" `Quick test_pbkdf2_rejects ] );
      ( "ed25519",
        [ Alcotest.test_case "RFC 8032 vector 1" `Quick test_rfc8032;
          Alcotest.test_case "rejections" `Quick test_ed25519_rejects ] );
      ( "mnemonics",
        List.mapi (fun i s -> Alcotest.test_case (Printf.sprintf "case %d" i) `Quick (mnemonic_case i s)) mnemonics
        @ List.mapi (fun i s -> Alcotest.test_case (Printf.sprintf "invalid %d" i) `Quick (invalid_case i s)) invalid
        @ [ Alcotest.test_case "password changes the key" `Quick test_password_changes_key;
            Alcotest.test_case "normalisation" `Quick test_normalisation ] )
    ]
