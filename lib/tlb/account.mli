(** Account state.

    {v
    account_none$0 = Account;
    account$1 addr:MsgAddressInt storage_stat:StorageInfo storage:AccountStorage = Account;

    storage_used$_ cells:(VarUInteger 7) bits:(VarUInteger 7) = StorageUsed;
    storage_extra_none$000 = StorageExtraInfo;
    storage_extra_info$001 dict_hash:uint256 = StorageExtraInfo;
    storage_info$_ used:StorageUsed storage_extra:StorageExtraInfo
                   last_paid:uint32 due_payment:(Maybe Grams) = StorageInfo;

    account_storage$_ last_trans_lt:uint64 balance:CurrencyCollection
                      state:AccountState = AccountStorage;
    account_uninit$00 = AccountState;
    account_active$1 _:StateInit = AccountState;
    account_frozen$01 state_hash:bits256 = AccountState;

    account_descr$_ account:^Account last_trans_hash:bits256
                    last_trans_lt:uint64 = ShardAccount;
    v}

    A note on schema drift: [StorageUsed] used to carry a third field,
    [public_cells:(VarUInteger 7)], where [storage_extra] now sits. Account
    states recorded before the change still parse under this schema, but only
    by coincidence — a [VarUInteger 7] holding zero is the three bits [000],
    which is exactly [storage_extra_none]. Any historical state with a
    non-zero [public_cells] would decode to nonsense. *)

open Ton_cell

type storage_used = { cells : Z.t; bits : Z.t }

type storage_info = {
  used : storage_used;
  storage_extra : Z.t option;  (** [dict_hash], when present. *)
  last_paid : int32;
  due_payment : Z.t option;
}

type state =
  | Uninit
  | Active of Message.state_init
  | Frozen of string  (** 32-byte state hash. *)

type storage = { last_trans_lt : int64; balance : Currency.t; state : state }
type t = { addr : Msg_address.t; storage_info : storage_info; storage : storage }

val load : Slice.t -> t option
(** [None] is [account_none], meaning the account does not exist. *)

val store : Builder.t -> t option -> (Builder.t, string) result
val of_cell : Cell.t -> (t option, Slice.error) result

(** {2 Convenience} *)

val address : t -> Ton_address.t option
val balance : t -> Z.t
(** The TON balance in nanotons. *)

val code : t -> Cell.t option
(** [None] unless the account is active. *)

val data : t -> Cell.t option
val is_active : t -> bool

(** {2 Shard accounts} *)

type shard = { account : t option; last_trans_hash : string; last_trans_lt : int64 }

val load_shard : Slice.t -> shard
val store_shard : Builder.t -> shard -> (Builder.t, string) result
