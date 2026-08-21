(** Hashing primitives.

    Thin wrappers over digestif, plus the PBKDF2 that TON's mnemonic scheme
    needs. Kept in one place so the rest of the library never reaches for a
    hash implementation directly. *)

val sha256 : string -> string
val sha512 : string -> string
val hmac_sha512 : key:string -> string -> string

val pbkdf2_sha512 : password:string -> salt:string -> iterations:int -> len:int -> string
(** PBKDF2 with HMAC-SHA-512, per RFC 8018.
    @raise Invalid_argument if [iterations] or [len] is not positive. *)
