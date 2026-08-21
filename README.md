# ocaml-ton

Pure-OCaml bindings for [TON](https://ton.org) (The Open Network), built to link
into a MirageOS/Solo5 unikernel.

TON is architecturally unlike the EVM chains: contracts are independent actors
exchanging asynchronous messages, and everything on chain — contract state,
message bodies, blocks, proofs — is encoded as a **Bag of Cells**, a DAG of
cells holding up to 1023 *bits* and up to 4 references each. Nothing about TON
is reachable from OCaml without that layer, and no bit-addressed codec existed.

**Status: early.** The cell and Bag-of-Cells layer works and is tested against
real mainnet data; everything above it is in progress.

| Package | What | Status |
| --- | --- | --- |
| `ton-cell` | Bits, cells, builder/slice, representation hash, exotic cells, BoC | working |
| `ton-address` | raw and user-friendly addresses | working |
| `ton-tlb` | dictionaries, coins, messages, accounts, VM stack | working |
| `ton-crypto` | Ed25519, mnemonics, Ed25519→X25519 | working |
| `ton-tl` / `ton-tl-schema` | TL wire runtime and generated liteserver schema | working |
| `ton-adnl` | ADNL over TCP, as a pure state machine | working |
| `ton-lite-client` | liteserver client, IO-free | working |
| `ton-lite-client-lwt` | Unix transport | working |
| `ton-wallet` | wallet v3R2 / v4R2 / v5R1 | working |

## Design

**The core is pure.** No `Unix`, no `Lwt`, no `Eio` anywhere below the IO
shims, so the library links into a unikernel unchanged. ADNL is written as a
`bytes in → bytes out` state machine rather than a functor over a flow, which
also makes an encrypted session replayable offline in tests.

**Untrusted input is treated as such.** A Bag of Cells arriving from a
liteserver is parsed with every index, width and checksum validated; decoding
returns a typed error and never raises.

**Two different cell hashes, deliberately.** `Cell.hash` (level 0) is the
*representation hash* that addresses, signatures and Merkle proofs are built
on. `Cell.identity` (level 3) is what distinguishes one cell from another. They
differ for pruned branches, which report the hash of the subtree they replace —
that is what makes Merkle proofs check out, and it means deduplicating on the
representation hash silently corrupts a proof. Note that ton-core's `hash()`
defaults to level 3, the opposite of ours.

## Building

Requires a switch with the reuna `mirage-crypto` fork and `web3-codec` pinned.
`ton-crypto` needs `Ed25519.Primitive.to_x25519_pub`, added to that fork for
this project; an unpatched mirage-crypto will not build it.


```sh
opam switch create ocaml-ton ocaml-base-compiler.5.2.1
eval $(opam env --switch=ocaml-ton)
for p in mirage-crypto mirage-crypto-rng mirage-crypto-ec; do
  opam pin add -k path -yn $p ../ocaml/mirage-crypto
done
opam pin add -k path -yn web3-codec ../ocaml-web3-codec
opam install -y . --deps-only --with-test
dune build && dune runtest
```

## Testing

`dune runtest` is offline and hermetic — no network, no Node.

Correctness is established differentially against the reference TypeScript SDK
rather than against the prose specification, which does not fully describe the
hashing rules. `test/vectors/ton-core/` holds real mainnet Bags of Cells copied
from `ton-org/ton-core`, and `test/vectors/ton-core-expected.json` records what
that implementation computes for each: root hashes at every level, depths, cell
counts, and digests of its re-serialized output. Our decoder must agree on all
of it, and our encoder must emit the same bytes.

Worth knowing: those fixtures were produced by the C++ node, and `@ton/core`
does not re-serialize them byte-for-byte either — both implementations differ
from the input at the same offsets. So "correct" here means "agrees with the
reference", not "reproduces the input".

Builder and address vectors work the same way. Builder cases are stored as
small *programs* of store operations that both implementations interpret, so
the two are running one spec rather than two hand transcriptions of it.

ADNL is tested by replaying a recorded mainnet session. Every client-side
random value — the ephemeral key, the session parameters, the query id, each
frame nonce — was fixed when the transcript was taken, so the client's half of
the conversation is a pure function of its inputs and must reproduce byte for
byte. That is what the sans-IO design buys: an encrypted protocol tested with
no socket. `tools/record_transcript/` takes a new one.

`test/live/live.exe` runs a real query against mainnet. It is an executable,
not a test, so `dune runtest` never touches the network.

`tools/gen-vectors/` regenerates the expectations (needs Node); the output is
committed so the suite never depends on it.

The TL bindings are generated rather than written, and the generated sources
are committed. `dune runtest` diffs them against a fresh run of the compiler,
so a stale file fails the build and `dune promote` fixes it. Every one of the
747 constructor identifiers in the two vendored schemas is cross-checked
against an independent derivation of the same rule.

## Layout

```
lib/cell/       bits, level masks, cell types, exotic cells, hashing, BoC,
                builder and slice cursors
lib/address/    raw and user-friendly addresses
lib/tlb/        hashmaps, coins, message addresses, state init, messages,
                account state, TVM stack values
lib/crypto/     hashing, Ed25519, X25519 agreement, TON mnemonics
lib/wallet/     wallet contracts and signed transfers
lib/tl/         the TL wire format (unrelated to TL-B)
lib/tl_schema/  generated liteserver and ADNL bindings, committed
lib/adnl/       ADNL handshake and framing, no IO
lib/lite_client/  typed liteserver queries and sessions, no IO
lib/io/lwt/     the only package that opens a socket
schema/         vendored .tl schemas, commit-pinned in PROVENANCE
tools/tlgen/    dev tool: the TL schema compiler
tools/record_transcript/  dev tool: record a live session for offline replay
test/live/      a real query against mainnet; not part of `dune runtest`
tools/bocinfo/  dev tool: describe a Bag of Cells and check it round-trips
tools/gen-vectors/  dev tool: regenerate cross-implementation expectations
test/           unit and property tests, plus the committed vectors
```
