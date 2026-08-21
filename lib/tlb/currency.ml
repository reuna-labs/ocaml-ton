type t = { coins : Z.t; extra : Z.t Dict.t }

let extra_key_bits = 32
let extra_value = 32
let empty_extra = Dict.empty ~key_bits:extra_key_bits
let zero = { coins = Z.zero; extra = empty_extra }
let of_coins coins = { coins; extra = empty_extra }

let load s =
  let coins = Coins.load_coins s in
  let extra = Dict.load_maybe s ~key_bits:extra_key_bits ~value:(fun s -> Coins.load_var_uint s ~n:extra_value) in
  { coins; extra }

let store b t =
  match Coins.store_coins b t.coins with
  | Error e -> Error (Format.asprintf "%a" Coins.pp_error e)
  | Ok b -> (
      let value b v =
        match Coins.store_var_uint b ~n:extra_value v with
        | Ok b -> b
        | Error e -> failwith (Format.asprintf "%a" Coins.pp_error e)
      in
      match Dict.store_maybe b ~value t.extra with
      | Ok b -> Ok b
      | Error e -> Error (Format.asprintf "%a" Dict.pp_error e))

let equal a b =
  Z.equal a.coins b.coins && Dict.to_list a.extra = Dict.to_list b.extra

let pp ppf t =
  Format.fprintf ppf "%s TON" (Coins.to_string t.coins);
  if not (Dict.is_empty t.extra) then Format.fprintf ppf " + %d extra" (Dict.cardinal t.extra)
