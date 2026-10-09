let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
let sha512 s = Digestif.SHA512.(to_raw_string (digest_string s))
let hmac_sha512 ~key s = Digestif.SHA512.(to_raw_string (hmac_string ~key s))

let pbkdf2_sha512 ~password ~salt ~iterations ~len =
  if iterations < 1 then
    invalid_arg "pbkdf2_sha512: iterations must be positive";
  if len < 1 then invalid_arg "pbkdf2_sha512: length must be positive";
  Mirage_crypto_pbkdf2.sha512 ~password ~salt ~iterations ~length:len
