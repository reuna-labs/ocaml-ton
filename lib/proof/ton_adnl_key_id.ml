(* The short identifier of an Ed25519 key: SHA-256 of the TL-boxed key. The
   same construction ADNL uses to name a peer, reproduced here so that
   verifying proofs does not drag in the network stack. The four-byte prefix
   is pub.ed25519's constructor identifier, little-endian. *)
let pub_ed25519_prefix = "\xc6\xb4\x13\x48"

let of_ed25519_pub key =
  if String.length key <> 32 then invalid_arg "key must be 32 bytes";
  Digestif.SHA256.(to_raw_string (digest_string (pub_ed25519_prefix ^ key)))
