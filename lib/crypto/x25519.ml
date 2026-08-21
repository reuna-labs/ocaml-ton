module P = Mirage_crypto_ec.Ed25519.Primitive

let of_ed25519_pub pub =
  if String.length pub <> 32 then Error "Ed25519 public key must be 32 bytes"
  else
    match P.to_x25519_pub pub with
    | Ok u -> Ok u
    | Error _ -> Error "Ed25519 public key has no Curve25519 image (y = 1)"

let scalar_of_ed25519_seed seed =
  if String.length seed <> 32 then invalid_arg "X25519.scalar_of_ed25519_seed: seed must be 32 bytes";
  P.to_x25519_priv seed

let key_exchange ~scalar ~peer =
  match Mirage_crypto_ec.X25519.secret_of_octets scalar with
  | Error e -> Error (Format.asprintf "%a" Mirage_crypto_ec.pp_error e)
  | Ok (secret, _) -> (
      match Mirage_crypto_ec.X25519.key_exchange secret peer with
      | Ok s -> Ok s
      | Error e -> Error (Format.asprintf "%a" Mirage_crypto_ec.pp_error e))
