(** A streaming AES-256-CTR keystream.

    ADNL runs one counter-mode stream across an entire connection, and its
    frames are not multiples of the block size, so the leftover of a partial
    block has to carry across calls. Getting that wrong desynchronises the
    stream a few bytes in and every subsequent frame decrypts to noise — a
    failure that looks like a protocol bug rather than a cipher bug.

    Values are immutable: applying the keystream returns the advanced state.
    That keeps a whole session reproducible from its inputs, which is what
    makes offline replay possible. *)

type t

val create : key:string -> iv:string -> t
(** @raise Invalid_argument unless [key] is 32 bytes and [iv] is 16. *)

val xor : t -> string -> t * string
(** Apply the keystream. Encryption and decryption are the same operation. *)

val offset : t -> int
(** Bytes consumed so far. Useful when comparing against a transcript. *)
