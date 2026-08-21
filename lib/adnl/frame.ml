type error =
  | Too_large of { size : int; limit : int }
  | Too_small of int
  | Bad_checksum

let pp_error ppf = function
  | Too_large { size; limit } -> Format.fprintf ppf "frame of %d bytes exceeds the %d byte limit" size limit
  | Too_small n -> Format.fprintf ppf "frame of %d bytes is smaller than the 64 byte overhead" n
  | Bad_checksum -> Format.fprintf ppf "frame checksum does not match"

let nonce_size = 32
let checksum_size = 32
let overhead = nonce_size + checksum_size
let default_max_size = 4 * 1024 * 1024
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
let le32 v = String.init 4 (fun i -> Char.unsafe_chr ((v lsr (8 * i)) land 0xff))

let encode ~nonce payload =
  if String.length nonce <> nonce_size then invalid_arg "Frame.encode: nonce must be 32 bytes";
  let body = nonce ^ payload in
  String.concat "" [ le32 (overhead + String.length payload); body; sha256 body ]

let decode ~max_size buf =
  let have = String.length buf in
  if have < 4 then Ok None
  else
    let size =
      Char.code buf.[0] lor (Char.code buf.[1] lsl 8) lor (Char.code buf.[2] lsl 16) lor (Char.code buf.[3] lsl 24)
    in
    if size < overhead then Error (Too_small size)
    else if size > max_size then Error (Too_large { size; limit = max_size })
    else if have < 4 + size then Ok None
    else
      let body = String.sub buf 4 (size - checksum_size) in
      let checksum = String.sub buf (4 + size - checksum_size) checksum_size in
      if not (String.equal (sha256 body) checksum) then Error Bad_checksum
      else Ok (Some (String.sub body nonce_size (String.length body - nonce_size), 4 + size))
