(** Wallet contracts.

    A TON wallet is an ordinary contract: its address is the hash of its
    initial state, so a key plus a version plus a workchain determines where
    the wallet lives before it has ever been deployed. Spending is done by
    sending the contract an {i external} message carrying a signed payload
    that it checks against the public key in its own data.

    The three supported versions differ in ways that matter:

    - {b v3R2} signs [wallet_id ‖ valid_until ‖ seqno] followed by up to four
      [(mode, message)] pairs, and puts the signature {i before} the payload.
    - {b v4R2} inserts an 8-bit opcode after the seqno. The documentation says
      this field is 32 bits; the contract source says 8, and the source wins.
    - {b v5R1} uses an opcode-tagged action list of up to 255 entries and puts
      the signature {i after} the payload.

    Wallet identifiers differ too. v3 and v4 use [698983191 + workchain]. v5R1
    uses [network_global_id XOR context_id], where the context packs the
    workchain, a version byte and a subwallet number — so a v5R1 wallet's
    workchain is part of its identity rather than merely its address. *)

open Ton_cell

type version = V3R2 | V4R2 | V5R1

val version_to_string : version -> string
val code : version -> Cell.t
(** The shared compiled code cell for a version. *)

type network = Mainnet | Testnet

type t

val create :
  ?workchain:int ->
  ?network:network ->
  ?subwallet:int ->
  ?wallet_id:int32 ->
  version ->
  public_key:string ->
  (t, string) result
(** [workchain] defaults to 0 and [network] to {!Mainnet}. [wallet_id]
    overrides the derived identifier, for wallets that were created with a
    non-standard one. [subwallet] applies to v5R1 only. *)

val version : t -> version
val workchain : t -> int
val public_key : t -> string
val wallet_id : t -> int32
val state_init : t -> Ton_tlb.Message.state_init
val address : t -> Ton_address.t

(** {2 Outgoing messages} *)

val internal :
  ?bounce:bool -> ?body:Cell.t -> ?init:Ton_tlb.Message.state_init -> dest:Ton_address.t ->
  value:Z.t -> unit -> Ton_tlb.Message.t
(** A relaxed internal message — one with no source address, which the
    validator fills in. This is what a wallet sends. *)

val send_mode_default : int
(** [3]: pay transfer fees separately and ignore errors. *)

val create_transfer :
  t -> key:Ton_crypto.Ed25519.t -> seqno:int -> valid_until:int32 -> ?send_mode:int ->
  Ton_tlb.Message.t list -> (Cell.t, string) result
(** The signed body to put in an external message.

    [valid_until] is an absolute Unix timestamp and is deliberately explicit:
    a wallet body is only replay-protected by [seqno] and this deadline, and
    the library has no clock. *)

val external_message : ?with_init:bool -> t -> body:Cell.t -> Ton_tlb.Message.t
(** Wrap a signed body in the external message addressed to the wallet.
    Set [with_init] when the wallet has not been deployed yet. *)

val max_messages : version -> int
(** [4] for v3 and v4, [255] for v5R1. *)
