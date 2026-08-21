type error =
  | Bad_server_key of string
  | Bad_seed of string
  | Bad_aes_params of int
  | Key_exchange of string

let pp_error ppf = function
  | Bad_server_key m -> Format.fprintf ppf "server key: %s" m
  | Bad_seed m -> Format.fprintf ppf "ephemeral seed: %s" m
  | Bad_aes_params n -> Format.fprintf ppf "session parameters must be 160 bytes, got %d" n
  | Key_exchange m -> Format.fprintf ppf "key exchange: %s" m

let packet_size = 256
let params_size = 160
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
let ( let* ) = Result.bind

(* The session parameters are laid out as two 32-byte keys followed by two
   16-byte counters, named from the client's point of view; the server reads
   the same bytes with the roles swapped. The remaining 64 bytes are padding. *)
let rx_key p = String.sub p 0 32
let tx_key p = String.sub p 32 32
let rx_iv p = String.sub p 64 16
let tx_iv p = String.sub p 80 16

let build ~server_pub ~ephemeral_seed ~aes_params =
  if String.length aes_params <> params_size then Error (Bad_aes_params (String.length aes_params))
  else
    let* peer =
      Result.map_error (fun m -> Bad_server_key m) (Ton_crypto.X25519.of_ed25519_pub server_pub)
    in
    let* keypair = Result.map_error (fun m -> Bad_seed m) (Ton_crypto.Ed25519.of_seed ephemeral_seed) in
    let scalar = Ton_crypto.X25519.scalar_of_ed25519_seed ephemeral_seed in
    let* secret = Result.map_error (fun m -> Key_exchange m) (Ton_crypto.X25519.key_exchange ~scalar ~peer) in
    let checksum = sha256 aes_params in
    (* The key and counter that protect the parameters are woven from halves
       of the shared secret and halves of their own checksum, so neither party
       can influence them alone. *)
    let key = String.sub secret 0 16 ^ String.sub checksum 16 16 in
    let iv = String.sub checksum 0 4 ^ String.sub secret 20 12 in
    let _, encrypted = Ctr.xor (Ctr.create ~key ~iv) aes_params in
    let packet =
      String.concat ""
        [ Key_id.of_ed25519_pub server_pub;
          Ton_crypto.Ed25519.public keypair;
          checksum;
          encrypted ]
    in
    assert (String.length packet = packet_size);
    Ok
      ( packet,
        Ctr.create ~key:(rx_key aes_params) ~iv:(rx_iv aes_params),
        Ctr.create ~key:(tx_key aes_params) ~iv:(tx_iv aes_params) )
