(** TVM stack values.

    This is the format [runSmcMethod] uses for both its arguments and its
    result, so it is the boundary between OCaml values and anything a contract
    computes.

    {v
    vm_stk_null#00 = VmStackValue;
    vm_stk_tinyint#01 value:int64 = VmStackValue;
    vm_stk_int#0201_ value:int257 = VmStackValue;
    vm_stk_nan#02ff = VmStackValue;
    vm_stk_cell#03 cell:^Cell = VmStackValue;
    vm_stk_slice#04 _:VmCellSlice = VmStackValue;
    vm_stk_builder#05 cell:^Cell = VmStackValue;
    vm_stk_tuple#07 len:(## 16) data:(VmTuple len) = VmStackValue;

    vm_stack#_ depth:(## 24) stack:(VmStackList depth) = VmStack;
    vm_stk_cons#_ {n:#} rest:^(VmStackList n) tos:VmStackValue = VmStackList (n+1);
    vm_stk_nil#_ = VmStackList 0;
    v}

    Two things about this encoding regularly catch implementations out. The
    stack is a cons list threaded {i backwards} through references — the rest
    of the stack is the reference and the top is inline — so it is
    reconstructed from the deepest cell outwards. And integers are encoded by
    magnitude: anything that fits [int64] uses the one-byte [vm_stk_tinyint]
    tag, everything else uses the fifteen-bit [vm_stk_int] prefix. A value's
    tag therefore depends on the value, and re-encoding is only stable because
    the choice is deterministic. *)

type item =
  | Null
  | Int of Z.t
  | Nan
  | Cell of Ton_cell.Cell.t
  | Slice of Ton_cell.Cell.t
  | Builder of Ton_cell.Cell.t
  | Tuple of item list

type t = item list
(** Top of stack first. *)

val store_item : Ton_cell.Builder.t -> item -> Ton_cell.Builder.t
val load_item : Ton_cell.Slice.t -> item
(** @raise Ton_cell.Slice.Parse_error on malformed input. *)

val to_cell : t -> (Ton_cell.Cell.t, string) result
val of_cell : Ton_cell.Cell.t -> (t, Ton_cell.Slice.error) result

val equal : item -> item -> bool
val pp : Format.formatter -> item -> unit
val pp_stack : Format.formatter -> t -> unit
