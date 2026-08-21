type t = { rx : Ctr.t; tx : Ctr.t; inbuf : string; confirmed : bool; max_frame : int }
type error = Handshake of Handshake.error | Frame of Frame.error

let pp_error ppf = function
  | Handshake e -> Handshake.pp_error ppf e
  | Frame e -> Frame.pp_error ppf e

let connect ?(max_frame = Frame.default_max_size) ~server_pub ~ephemeral_seed ~aes_params () =
  match Handshake.build ~server_pub ~ephemeral_seed ~aes_params with
  | Error e -> Error (Handshake e)
  | Ok (packet, rx, tx) -> Ok ({ rx; tx; inbuf = ""; confirmed = false; max_frame }, packet)

let send t ~nonce payload =
  let tx, out = Ctr.xor t.tx (Frame.encode ~nonce payload) in
  ({ t with tx }, out)

let recv t data =
  (* Counter mode is a stream, so incoming bytes are decrypted as they arrive
     and the frame boundaries are found afterwards in the plaintext. *)
  let rx, plain = Ctr.xor t.rx data in
  let buf = t.inbuf ^ plain in
  let rec take buf acc =
    match Frame.decode ~max_size:t.max_frame buf with
    | Error e -> Error (Frame e)
    | Ok None -> Ok (buf, List.rev acc)
    | Ok (Some (payload, used)) ->
        take (String.sub buf used (String.length buf - used)) (payload :: acc)
  in
  match take buf [] with
  | Error _ as e -> e
  | Ok (inbuf, frames) ->
      Ok ({ t with rx; inbuf; confirmed = (t.confirmed || frames <> []) }, frames)

let confirmed t = t.confirmed
let buffered t = String.length t.inbuf
