open Ton_cell

type tick_tock = { tick : bool; tock : bool }

type state_init = {
  split_depth : int option;
  special : tick_tock option;
  code : Cell.t option;
  data : Cell.t option;
  library : Cell.t option;
}

let empty_state_init =
  { split_depth = None; special = None; code = None; data = None; library = None }

let load_maybe s f = if Slice.load_bit s then Some (f s) else None

let store_maybe b f = function
  | None -> Builder.store_bit b false
  | Some v -> f (Builder.store_bit b true) v

let load_state_init s =
  let split_depth = load_maybe s (fun s -> Int64.to_int (Slice.load_uint s ~bits:5)) in
  let special =
    load_maybe s (fun s ->
        let tick = Slice.load_bit s in
        let tock = Slice.load_bit s in
        { tick; tock })
  in
  let code = Slice.load_maybe_ref s in
  let data = Slice.load_maybe_ref s in
  let library = Slice.load_maybe_ref s in
  { split_depth; special; code; data; library }

let store_state_init b t =
  let b = store_maybe b (fun b d -> Builder.store_uint b (Int64.of_int d) ~bits:5) t.split_depth in
  let b = store_maybe b (fun b { tick; tock } -> Builder.store_bit (Builder.store_bit b tick) tock) t.special in
  let b = Builder.store_maybe_ref b t.code in
  let b = Builder.store_maybe_ref b t.data in
  Builder.store_maybe_ref b t.library

let state_init_address ~workchain t =
  match Builder.end_cell (store_state_init (Builder.create ()) t) with
  | Error e -> Error (Format.asprintf "%a" Builder.pp_error e)
  | Ok c -> (
      match Ton_address.make ~workchain ~hash:(Cell.hash c) with
      | Ok a -> Ok a
      | Error e -> Error (Format.asprintf "%a" Ton_address.pp_error e))

type info =
  | Internal of {
      ihr_disabled : bool;
      bounce : bool;
      bounced : bool;
      src : Msg_address.t;
      dest : Msg_address.t;
      value : Currency.t;
      ihr_fee : Z.t;
      fwd_fee : Z.t;
      created_lt : int64;
      created_at : int32;
    }
  | External_in of { src : Msg_address.t; dest : Msg_address.t; import_fee : Z.t }
  | External_out of {
      src : Msg_address.t;
      dest : Msg_address.t;
      created_lt : int64;
      created_at : int32;
    }

let load_info s =
  if not (Slice.load_bit s) then begin
    (* int_msg_info$0 *)
    let ihr_disabled = Slice.load_bit s in
    let bounce = Slice.load_bit s in
    let bounced = Slice.load_bit s in
    let src = Msg_address.load s in
    let dest = Msg_address.load s in
    let value = Currency.load s in
    let ihr_fee = Coins.load_coins s in
    let fwd_fee = Coins.load_coins s in
    let created_lt = Slice.load_uint s ~bits:64 in
    let created_at = Int64.to_int32 (Slice.load_uint s ~bits:32) in
    Internal { ihr_disabled; bounce; bounced; src; dest; value; ihr_fee; fwd_fee; created_lt; created_at }
  end
  else if not (Slice.load_bit s) then begin
    (* ext_in_msg_info$10 *)
    let src = Msg_address.load s in
    let dest = Msg_address.load s in
    External_in { src; dest; import_fee = Coins.load_coins s }
  end
  else begin
    (* ext_out_msg_info$11 *)
    let src = Msg_address.load s in
    let dest = Msg_address.load s in
    let created_lt = Slice.load_uint s ~bits:64 in
    let created_at = Int64.to_int32 (Slice.load_uint s ~bits:32) in
    External_out { src; dest; created_lt; created_at }
  end

let ( let* ) = Result.bind
let coins b v = Result.map_error (fun e -> Format.asprintf "%a" Coins.pp_error e) (Coins.store_coins b v)

let store_info b = function
  | Internal i ->
      let b = Builder.store_bit b false in
      let b = Builder.store_bit b i.ihr_disabled in
      let b = Builder.store_bit b i.bounce in
      let b = Builder.store_bit b i.bounced in
      let b = Msg_address.store b i.src in
      let b = Msg_address.store b i.dest in
      let* b = Currency.store b i.value in
      let* b = coins b i.ihr_fee in
      let* b = coins b i.fwd_fee in
      let b = Builder.store_uint b i.created_lt ~bits:64 in
      Ok (Builder.store_uint b (Int64.of_int32 i.created_at) ~bits:32)
  | External_in i ->
      let b = Builder.store_uint b 2L ~bits:2 in
      let b = Msg_address.store b i.src in
      let b = Msg_address.store b i.dest in
      coins b i.import_fee
  | External_out i ->
      let b = Builder.store_uint b 3L ~bits:2 in
      let b = Msg_address.store b i.src in
      let b = Msg_address.store b i.dest in
      let b = Builder.store_uint b i.created_lt ~bits:64 in
      Ok (Builder.store_uint b (Int64.of_int32 i.created_at) ~bits:32)

type t = { info : info; init : state_init option; body : Cell.t }

let load s =
  let info = load_info s in
  let init =
    if not (Slice.load_bit s) then None
    else if Slice.load_bit s then Some (load_state_init (Slice.of_cell (Slice.load_ref s)))
    else Some (load_state_init s)
  in
  (* Either X ^X: inline, or in a reference. *)
  let body =
    if Slice.load_bit s then Slice.load_ref s
    else
      let rest = Slice.to_bits s in
      let b = Builder.store_bits (Builder.create ()) rest in
      let b = List.fold_left Builder.store_ref b (Slice.refs s) in
      Slice.skip s (Bits.length rest);
      (match Builder.end_cell b with
      | Ok c -> c
      | Error e -> Slice.fail (Slice.Message (Format.asprintf "%a" Builder.pp_error e)))
  in
  { info; init; body }

let cell_of_builder b =
  Result.map_error (fun e -> Format.asprintf "%a" Builder.pp_error e) (Builder.end_cell b)

let store b t =
  let* b = store_info b t.info in
  (* State init goes inline when there is room, otherwise into a reference. *)
  let* b =
    match t.init with
    | None -> Ok (Builder.store_bit b false)
    | Some init ->
        let inline = store_state_init (Builder.create ()) init in
        let fits =
          Builder.error inline = None
          && Builder.bit_length inline + 2 <= Builder.available_bits b
          && Builder.ref_count inline + 1 <= Builder.available_refs b
        in
        if fits then Ok (store_state_init (Builder.store_bit (Builder.store_bit b true) false) init)
        else
          let* c = cell_of_builder inline in
          Ok (Builder.store_ref (Builder.store_bit (Builder.store_bit b true) true) c)
  in
  let body_bits = Bits.length (Cell.bits t.body) and body_refs = Cell.ref_count t.body in
  if body_bits + 1 <= Builder.available_bits b && body_refs <= Builder.available_refs b then
    (* Inline: the body's own bits and refs are spliced in. *)
    let b = Builder.store_bit b false in
    let b = Builder.store_bits b (Cell.bits t.body) in
    Ok (List.fold_left Builder.store_ref b (Cell.refs t.body))
  else Ok (Builder.store_ref (Builder.store_bit b true) t.body)

let to_cell t =
  let* b = store (Builder.create ()) t in
  cell_of_builder b

let of_cell c =
  Slice.parse c (fun s ->
      let m = load s in
      m)
