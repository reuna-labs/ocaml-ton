(* The identifier hashes the TL-boxed key, so the four-byte pub.ed25519
   constructor prefix is part of the input. It is taken from the generated
   schema rather than written out, so it cannot drift from the schema. *)
let of_ed25519_pub key =
  if String.length key <> 32 then invalid_arg "Key_id.of_ed25519_pub: key must be 32 bytes";
  let boxed =
    Ton_tl.Tl.Writer.to_string (fun w -> Ton_tl_schema.Adnl.write_boxed_pub_ed25519 w { key })
  in
  Digestif.SHA256.(to_raw_string (digest_string boxed))
