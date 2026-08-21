let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
let sha512 s = Digestif.SHA512.(to_raw_string (digest_string s))
let hmac_sha512 ~key s = Digestif.SHA512.(to_raw_string (hmac_string ~key s))

let xor_into dst src =
  String.iteri
    (fun i c -> Bytes.unsafe_set dst i (Char.unsafe_chr (Char.code (Bytes.unsafe_get dst i) lxor Char.code c)))
    src

let pbkdf2_sha512 ~password ~salt ~iterations ~len =
  if iterations < 1 then invalid_arg "pbkdf2_sha512: iterations must be positive";
  if len < 1 then invalid_arg "pbkdf2_sha512: length must be positive";
  let h_len = 64 in
  let blocks = (len + h_len - 1) / h_len in
  let out = Buffer.create (blocks * h_len) in
  for i = 1 to blocks do
    (* U1 = PRF(P, S || INT_BE(i)); U_n = PRF(P, U_{n-1}); T = U1 xor .. xor Uc *)
    let counter = String.init 4 (fun k -> Char.unsafe_chr ((i lsr (8 * (3 - k))) land 0xff)) in
    let u = ref (hmac_sha512 ~key:password (salt ^ counter)) in
    let acc = Bytes.of_string !u in
    for _ = 2 to iterations do
      u := hmac_sha512 ~key:password !u;
      xor_into acc !u
    done;
    Buffer.add_bytes out acc
  done;
  String.sub (Buffer.contents out) 0 len
