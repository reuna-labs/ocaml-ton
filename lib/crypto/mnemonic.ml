let word_count = Wordlist.count
let words = Wordlist.words

(* 2048 words, looked up often enough during validation to be worth a table. *)
let index =
  lazy
    (let h = Hashtbl.create 4096 in
     Array.iter (fun w -> Hashtbl.replace h w ()) words;
     h)

let is_word w = Hashtbl.mem (Lazy.force index) w
let normalize l = List.map (fun w -> String.lowercase_ascii (String.trim w)) l

let of_string s =
  List.filter (fun w -> w <> "") (String.split_on_char ' ' (String.map (function '\t' | '\n' | '\r' -> ' ' | c -> c) s))

let to_string l = String.concat " " l

let pbkdf_iterations = 100_000

(* The mnemonic is the HMAC key and the password is the message. That is the
   order tonlib uses; reversing it silently produces a different wallet. *)
let to_entropy ?(password = "") l = Hash.hmac_sha512 ~key:(to_string l) password

let to_seed ?password ?(salt = "TON default seed") l =
  Hash.pbkdf2_sha512 ~password:(to_entropy ?password l) ~salt ~iterations:pbkdf_iterations ~len:64

let to_keypair ?password l =
  let l = normalize l in
  Ed25519.of_seed (String.sub (to_seed ?password l) 0 32)

let is_basic_seed entropy =
  let s =
    Hash.pbkdf2_sha512 ~password:entropy ~salt:"TON seed version"
      ~iterations:(max 1 (pbkdf_iterations / 256))
      ~len:64
  in
  Char.code s.[0] = 0

let is_password_seed entropy =
  let s = Hash.pbkdf2_sha512 ~password:entropy ~salt:"TON fast seed version" ~iterations:1 ~len:64 in
  Char.code s.[0] = 1

let is_password_needed l =
  let e = to_entropy l in
  is_password_seed e && not (is_basic_seed e)

let validate ?password l =
  let l = normalize l in
  List.for_all is_word l
  && (match password with
     | Some p when p <> "" -> is_password_needed l
     | _ -> true)
  && is_basic_seed (to_entropy ?password l)
