module E = Mirage_crypto_ec.Ed25519

type t = { seed : string; priv : E.priv; public : string }

let of_seed s =
  if String.length s <> 32 then
    Error (Printf.sprintf "Ed25519 seed must be 32 bytes, got %d" (String.length s))
  else
    match E.priv_of_octets s with
    | Ok priv -> Ok { seed = s; priv; public = E.pub_to_octets (E.pub_of_priv priv) }
    | Error e -> Error (Format.asprintf "%a" Mirage_crypto_ec.pp_error e)

let seed t = t.seed
let public t = t.public
let secret_key t = t.seed ^ t.public
let sign t msg = E.sign ~key:t.priv msg

let verify ~public ~signature msg =
  String.length signature = 64
  && match E.pub_of_octets public with Ok key -> E.verify ~key signature ~msg | Error _ -> false
