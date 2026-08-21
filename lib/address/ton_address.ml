type t = { workchain : int; hash : string }

type error =
  | Bad_raw of string
  | Bad_base64
  | Bad_length of int
  | Bad_tag of int
  | Bad_crc of { expected : string; got : string }

let basechain = 0
let masterchain = -1
let hash_length = 32

(* tag ‖ workchain ‖ hash ‖ crc16 *)
let friendly_length = 1 + 1 + hash_length + 2
let tag_bounceable = 0x11
let tag_non_bounceable = 0x51
let tag_testnet = 0x80

let pp_error ppf = function
  | Bad_raw s -> Format.fprintf ppf "not a raw address: %S" s
  | Bad_base64 -> Format.fprintf ppf "not valid base64"
  | Bad_length n -> Format.fprintf ppf "expected %d bytes, got %d" friendly_length n
  | Bad_tag n -> Format.fprintf ppf "unknown address tag 0x%02x" n
  | Bad_crc { expected; got } ->
      Format.fprintf ppf "CRC-16 mismatch: expected %s, computed %s" expected got

let hex s =
  String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let make ~workchain ~hash =
  if String.length hash <> hash_length then Error (Bad_length (String.length hash))
  else Ok { workchain; hash }

(* --- raw ------------------------------------------------------------------ *)

let unhex s =
  let n = String.length s in
  if n land 1 <> 0 then None
  else
    let digit c =
      match c with
      | '0' .. '9' -> Some (Char.code c - 48)
      | 'a' .. 'f' -> Some (Char.code c - 87)
      | 'A' .. 'F' -> Some (Char.code c - 55)
      | _ -> None
    in
    let buf = Bytes.create (n / 2) in
    let rec go i =
      if i >= n / 2 then Some (Bytes.unsafe_to_string buf)
      else
        match (digit s.[2 * i], digit s.[(2 * i) + 1]) with
        | Some a, Some b ->
            Bytes.unsafe_set buf i (Char.unsafe_chr ((a lsl 4) lor b));
            go (i + 1)
        | _ -> None
    in
    go 0

let of_raw s =
  match String.index_opt s ':' with
  | None -> Error (Bad_raw s)
  | Some i -> (
      let wc = String.sub s 0 i and rest = String.sub s (i + 1) (String.length s - i - 1) in
      match (int_of_string_opt wc, unhex rest) with
      | Some workchain, Some hash when String.length hash = hash_length -> Ok { workchain; hash }
      | _ -> Error (Bad_raw s))

let to_raw a = Printf.sprintf "%d:%s" a.workchain (hex a.hash)

(* --- user-friendly -------------------------------------------------------- *)

type friendly = { address : t; bounceable : bool; testnet : bool }

(* Normalising the URL-safe alphabet to the standard one means one decoder
   handles both, and mixed input too. *)
let normalise_base64 s =
  String.map (function '-' -> '+' | '_' -> '/' | c -> c) s

let of_friendly s =
  match Base64.decode (normalise_base64 (String.trim s)) with
  | Error (`Msg _) -> Error Bad_base64
  | Ok raw ->
      let n = String.length raw in
      if n <> friendly_length then Error (Bad_length n)
      else
        let body = String.sub raw 0 (friendly_length - 2) in
        let expected = String.sub raw (friendly_length - 2) 2 in
        let got = Web3_codec.Crc.crc16_xmodem_be body in
        if not (String.equal expected got) then
          Error (Bad_crc { expected = hex expected; got = hex got })
        else
          let tag = Char.code raw.[0] in
          let testnet = tag land tag_testnet <> 0 in
          let base = tag land lnot tag_testnet in
          if base <> tag_bounceable && base <> tag_non_bounceable then Error (Bad_tag tag)
          else
            (* The workchain byte is signed, so the masterchain (-1) arrives
               as 0xff. *)
            let b = Char.code raw.[1] in
            let workchain = if b >= 0x80 then b - 0x100 else b in
            Ok
              { address = { workchain; hash = String.sub raw 2 hash_length };
                bounceable = base = tag_bounceable;
                testnet }

let to_friendly ?(bounceable = true) ?(testnet = false) ?(url_safe = true) a =
  let tag =
    (if bounceable then tag_bounceable else tag_non_bounceable) lor if testnet then tag_testnet else 0
  in
  let body =
    String.concat ""
      [ String.make 1 (Char.chr tag); String.make 1 (Char.chr (a.workchain land 0xff)); a.hash ]
  in
  let raw = body ^ Web3_codec.Crc.crc16_xmodem_be body in
  Base64.encode_string ~alphabet:(if url_safe then Base64.uri_safe_alphabet else Base64.default_alphabet) raw

let of_string s =
  if String.contains s ':' then of_raw s
  else match of_friendly s with Ok f -> Ok f.address | Error e -> Error e

let equal a b = a.workchain = b.workchain && String.equal a.hash b.hash

let compare a b =
  match Int.compare a.workchain b.workchain with 0 -> String.compare a.hash b.hash | c -> c

let pp ppf a = Format.pp_print_string ppf (to_raw a)
