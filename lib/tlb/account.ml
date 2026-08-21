open Ton_cell

type storage_used = { cells : Z.t; bits : Z.t }

type storage_info = {
  used : storage_used;
  storage_extra : Z.t option;
  last_paid : int32;
  due_payment : Z.t option;
}

type state = Uninit | Active of Message.state_init | Frozen of string
type storage = { last_trans_lt : int64; balance : Currency.t; state : state }
type t = { addr : Msg_address.t; storage_info : storage_info; storage : storage }

let ( let* ) = Result.bind
let coins_err e = Format.asprintf "%a" Coins.pp_error e

let load_storage_used s =
  let cells = Coins.load_var_uint s ~n:7 in
  let bits = Coins.load_var_uint s ~n:7 in
  { cells; bits }

let store_storage_used b u =
  let* b = Result.map_error coins_err (Coins.store_var_uint b ~n:7 u.cells) in
  Result.map_error coins_err (Coins.store_var_uint b ~n:7 u.bits)

let load_storage_extra s =
  match Int64.to_int (Slice.load_uint s ~bits:3) with
  | 0 -> None
  | 1 -> Some (Slice.load_uint_z s ~bits:256)
  | n -> Slice.fail (Slice.Message (Printf.sprintf "unknown storage extra info header %d" n))

let store_storage_extra b = function
  | None -> Builder.store_uint b 0L ~bits:3
  | Some h -> Builder.store_uint_z (Builder.store_uint b 1L ~bits:3) h ~bits:256

let load_maybe_coins s = if Slice.load_bit s then Some (Coins.load_coins s) else None

let store_maybe_coins b = function
  | None -> Ok (Builder.store_bit b false)
  | Some v -> Result.map_error coins_err (Coins.store_coins (Builder.store_bit b true) v)

let load_storage_info s =
  let used = load_storage_used s in
  let storage_extra = load_storage_extra s in
  let last_paid = Int64.to_int32 (Slice.load_uint s ~bits:32) in
  let due_payment = load_maybe_coins s in
  { used; storage_extra; last_paid; due_payment }

let store_storage_info b i =
  let* b = store_storage_used b i.used in
  let b = store_storage_extra b i.storage_extra in
  let b = Builder.store_uint b (Int64.of_int32 i.last_paid) ~bits:32 in
  store_maybe_coins b i.due_payment

let load_state s =
  if Slice.load_bit s then Active (Message.load_state_init s)
  else if Slice.load_bit s then Frozen (Slice.load_bytes s 32)
  else Uninit

let store_state b = function
  | Active si -> Message.store_state_init (Builder.store_bit b true) si
  | Frozen h -> Builder.store_bytes (Builder.store_bit (Builder.store_bit b false) true) h
  | Uninit -> Builder.store_bit (Builder.store_bit b false) false

let load_storage s =
  let last_trans_lt = Slice.load_uint s ~bits:64 in
  let balance = Currency.load s in
  let state = load_state s in
  { last_trans_lt; balance; state }

let store_storage b st =
  let b = Builder.store_uint b st.last_trans_lt ~bits:64 in
  let* b = Currency.store b st.balance in
  Ok (store_state b st.state)

let load s =
  if not (Slice.load_bit s) then None
  else
    let addr = Msg_address.load s in
    let storage_info = load_storage_info s in
    let storage = load_storage s in
    Some { addr; storage_info; storage }

let store b = function
  | None -> Ok (Builder.store_bit b false)
  | Some a ->
      let b = Msg_address.store (Builder.store_bit b true) a.addr in
      let* b = store_storage_info b a.storage_info in
      store_storage b a.storage

let of_cell c = Slice.parse c (fun s -> load s)

let address a = Msg_address.to_address a.addr
let balance a = a.storage.balance.Currency.coins
let code a = match a.storage.state with Active si -> si.Message.code | _ -> None
let data a = match a.storage.state with Active si -> si.Message.data | _ -> None
let is_active a = match a.storage.state with Active _ -> true | _ -> false

type shard = { account : t option; last_trans_hash : string; last_trans_lt : int64 }

let load_shard s =
  let account = load (Slice.of_cell (Slice.load_ref s)) in
  let last_trans_hash = Slice.load_bytes s 32 in
  let last_trans_lt = Slice.load_uint s ~bits:64 in
  { account; last_trans_hash; last_trans_lt }

let store_shard b sh =
  let* inner = store (Builder.create ()) sh.account in
  let* c = Result.map_error (fun e -> Format.asprintf "%a" Builder.pp_error e) (Builder.end_cell inner) in
  let b = Builder.store_ref b c in
  let b = Builder.store_bytes b sh.last_trans_hash in
  Ok (Builder.store_uint b sh.last_trans_lt ~bits:64)
