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
| `ton-crypto` | Ed25519, mnemonics | working |
| `ton-tl` / `ton-tl-schema` | TL wire runtime and generated liteserver schema | planned |
| `ton-adnl` | ADNL over TCP, as a pure state machine | planned |
| `ton-lite-client` | liteserver client, IO-free | planned |
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

Requires a switch with the reuna `mirage-crypto` fork and `web3-codec` pinned:

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

`tools/gen-vectors/` regenerates the expectations (needs Node); the output is
committed so the suite never depends on it.

## Layout

```
lib/cell/       bits, level masks, cell types, exotic cells, hashing, BoC,
                builder and slice cursors
lib/address/    raw and user-friendly addresses
lib/tlb/        hashmaps, coins, message addresses, state init, messages,
                account state, TVM stack values
lib/crypto/     hashing, Ed25519, TON mnemonics
lib/wallet/     wallet contracts and signed transfers
tools/bocinfo/  dev tool: describe a Bag of Cells and check it round-trips
tools/gen-vectors/  dev tool: regenerate cross-implementation expectations
test/           unit and property tests, plus the committed vectors
```
