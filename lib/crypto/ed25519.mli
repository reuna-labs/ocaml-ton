(** Ed25519 keys.

    A TON key pair is derived from a 32-byte seed. Other SDKs, following
    NaCl, call the 64-byte [seed ‖ public] concatenation the "secret key";
    {!secret_key} produces that form for cross-checking, but only the seed is
    actually secret material. *)

type t

val of_seed : string -> (t, string) result
(** @return [Error] unless [seed] is exactly 32 bytes. *)

val seed : t -> string
val public : t -> string
(** The 32-byte public key. *)

val secret_key : t -> string
(** The 64-byte NaCl-style [seed ‖ public]. *)

val sign : t -> string -> string
(** A 64-byte signature. *)

val verify : public:string -> signature:string -> string -> bool
(** [false] rather than an error for a malformed key or signature: a caller
    checking a signature wants a yes or no. *)
