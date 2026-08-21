# ton-light-client unikernel

A TON light client that runs as a MirageOS/Solo5 unikernel. It connects to a
liteserver, asks for the masterchain head and an account, and verifies the
answer against the block instead of trusting it.

```sh
opam pin add -k path -yn web3-codec ../../ocaml-web3-codec
for p in mirage-crypto mirage-crypto-rng mirage-crypto-ec; do
  opam pin add -k path -yn $p ../../ocaml/mirage-crypto
done
opam pin add -k path -yn ton-cell .. && opam pin add -k path -yn ton-tlb ..
# …and the other ton-* packages, all from the repository root

mirage configure -t hvt
make depends
make
solo5-hvt --net:service=tap0 dist/ton-light-client.hvt
```

Substitute `-t unix` for a native binary, which is the quickest way to check
the unikernel code itself before fighting a cross toolchain.

Nothing protocol-shaped lives in this directory. `unikernel.ml` is plumbing:
open a flow, hand it to `Ton_lite_client_mirage`, print what comes back. Every
line that parses a cell, checks a proof or drives ADNL is the same pure code
the offline test suite runs, which is what the no-IO rule in the core buys.
