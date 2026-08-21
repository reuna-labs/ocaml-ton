(** TON mnemonics.

    TON uses the BIP-39 English wordlist but {b not} BIP-39 itself: there is
    no checksum over the word indices. Instead a phrase is valid when a
    derived hash happens to start with a zero byte, so generating a mnemonic
    means retrying until one does. That also means any 24 words from the list
    may or may not be a valid phrase, and the only way to find out is to run
    the derivation.

    {v
    entropy = HMAC-SHA-512(key = words joined by " ", data = password)
    seed    = PBKDF2-HMAC-SHA-512(entropy, "TON default seed", 100000, 64)
    key     = Ed25519 seed = seed[0..32]
    v}

    Note the argument order: the {i mnemonic} is the HMAC key and the
    {i password} is the message, which is the reverse of what one would
    expect. *)

val word_count : int
(** [2048]. *)

val words : string array
val is_word : string -> bool
val normalize : string list -> string list
(** Lowercases and trims, as the reference implementation does before any
    derivation. *)

val of_string : string -> string list
(** Splits on whitespace. *)

val to_string : string list -> string

val to_entropy : ?password:string -> string list -> string
val to_seed : ?password:string -> ?salt:string -> string list -> string
(** [salt] defaults to ["TON default seed"]; pass ["TON HD Keys seed"] for the
    hierarchical-deterministic seed. *)

val to_keypair : ?password:string -> string list -> (Ed25519.t, string) result

val is_basic_seed : string -> bool
(** Whether an entropy value yields a valid phrase — the first byte of
    [PBKDF2(entropy, "TON seed version", 390, 64)] is zero. *)

val is_password_seed : string -> bool
val is_password_needed : string list -> bool
(** Whether the phrase is only valid {i with} a password. *)

val validate : ?password:string -> string list -> bool
(** Every word is in the list, the password requirement is consistent, and the
    derived seed is a basic seed. *)
