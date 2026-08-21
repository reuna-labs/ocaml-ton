open Ton_cell

type anycast = { depth : int; rewrite_pfx : Bits.t }

type t =
  | Addr_none
  | Addr_extern of Bits.t
  | Addr_std of { anycast : anycast option; workchain : int; address : string }
  | Addr_var of { anycast : anycast option; workchain : int; address : Bits.t }

(* depth is [#<= 30], so five bits. *)
let anycast_depth_bits = 5

let of_address (a : Ton_address.t) =
  Addr_std { anycast = None; workchain = a.workchain; address = a.hash }

let to_address = function
  | Addr_std { workchain; address; _ } -> Some { Ton_address.workchain; hash = address }
  | Addr_var { workchain; address; _ } when Bits.length address = 256 ->
      Option.map (fun hash -> { Ton_address.workchain; hash }) (Bits.to_bytes address)
  | _ -> None

let load_anycast s =
  if not (Slice.load_bit s) then None
  else
    let depth = Int64.to_int (Slice.load_uint s ~bits:anycast_depth_bits) in
    if depth < 1 || depth > 30 then Slice.fail (Slice.Message "anycast depth must be 1..30");
    Some { depth; rewrite_pfx = Slice.load_bits s depth }

let store_anycast b = function
  | None -> Builder.store_bit b false
  | Some { depth; rewrite_pfx } ->
      let b = Builder.store_bit b true in
      Builder.store_bits (Builder.store_uint b (Int64.of_int depth) ~bits:anycast_depth_bits) rewrite_pfx

let load s =
  match Int64.to_int (Slice.load_uint s ~bits:2) with
  | 0 -> Addr_none
  | 1 ->
      let len = Int64.to_int (Slice.load_uint s ~bits:9) in
      Addr_extern (Slice.load_bits s len)
  | 2 ->
      let anycast = load_anycast s in
      let workchain = Int64.to_int (Slice.load_int s ~bits:8) in
      Addr_std { anycast; workchain; address = Slice.load_bytes s 32 }
  | _ ->
      let anycast = load_anycast s in
      let len = Int64.to_int (Slice.load_uint s ~bits:9) in
      let workchain = Int64.to_int (Slice.load_int s ~bits:32) in
      Addr_var { anycast; workchain; address = Slice.load_bits s len }

let store b = function
  | Addr_none -> Builder.store_uint b 0L ~bits:2
  | Addr_extern bits ->
      let b = Builder.store_uint b 1L ~bits:2 in
      let b = Builder.store_uint b (Int64.of_int (Bits.length bits)) ~bits:9 in
      Builder.store_bits b bits
  | Addr_std { anycast; workchain; address } ->
      let b = Builder.store_uint b 2L ~bits:2 in
      let b = store_anycast b anycast in
      let b = Builder.store_int b (Int64.of_int workchain) ~bits:8 in
      Builder.store_bytes b address
  | Addr_var { anycast; workchain; address } ->
      let b = Builder.store_uint b 3L ~bits:2 in
      let b = store_anycast b anycast in
      let b = Builder.store_uint b (Int64.of_int (Bits.length address)) ~bits:9 in
      let b = Builder.store_int b (Int64.of_int workchain) ~bits:32 in
      Builder.store_bits b address

let equal_anycast a b =
  match (a, b) with
  | None, None -> true
  | Some x, Some y -> x.depth = y.depth && Bits.equal x.rewrite_pfx y.rewrite_pfx
  | _ -> false

let equal a b =
  match (a, b) with
  | Addr_none, Addr_none -> true
  | Addr_extern x, Addr_extern y -> Bits.equal x y
  | Addr_std x, Addr_std y ->
      equal_anycast x.anycast y.anycast && x.workchain = y.workchain
      && String.equal x.address y.address
  | Addr_var x, Addr_var y ->
      equal_anycast x.anycast y.anycast && x.workchain = y.workchain && Bits.equal x.address y.address
  | _ -> false

let pp ppf = function
  | Addr_none -> Format.pp_print_string ppf "addr_none"
  | Addr_extern b -> Format.fprintf ppf "addr_extern:%a" Bits.pp b
  | Addr_std { workchain; address; anycast } ->
      Format.fprintf ppf "%d:%s%s" workchain
        (String.concat "" (List.init (String.length address) (fun i -> Printf.sprintf "%02x" (Char.code address.[i]))))
        (if anycast = None then "" else " (anycast)")
  | Addr_var { workchain; address; _ } -> Format.fprintf ppf "addr_var %d:%a" workchain Bits.pp address
