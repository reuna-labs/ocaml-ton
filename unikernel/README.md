# ton-light-client unikernel

A TON light client as a MirageOS unikernel. It connects to a liteserver, asks
for the masterchain head and an account, and verifies the answer against the
block instead of trusting it.

Nothing protocol-shaped lives here. `unikernel.ml` is the MirageOS entry
point and does nothing but read configuration and hand a flow to
`Ton_light_client`, which lives in `lib/light_client/` so that it is
type-checked by the ordinary build rather than only when a cross toolchain is
present. Every line that parses a cell, checks a proof or drives ADNL is the
same pure code the offline test suite runs.

## Building

The packages have to be pinned from the repository root first:

```sh
cd ..
opam pin add -k path -yn web3-codec ../ocaml-web3-codec
for p in mirage-crypto mirage-crypto-rng mirage-crypto-ec; do
  opam pin add -k path -yn $p ../../ports/ocaml/mirage-crypto
done
for p in ton-cell ton-address ton-tlb ton-crypto ton-tl ton-tl-schema ton-adnl \
         ton-lite-client ton-lite-client-mirage ton-proof ton-wallet ton-light-client; do
  opam pin add -k path -yn $p .
done
```

Then, in this directory:

```sh
mirage configure -t unix     # or -t hvt for Solo5
make depends
make
```

## Status

`mirage configure` succeeds for both targets and the unikernel's own code
compiles against the real MirageOS TCP stack — `dune build` at the repository
root type-checks `lib/light_client/` against `Tcpip.Stack.V4V6`.

`make depends` does **not** currently complete in the `ocaml-ton` switch.
Mirage vendors dependencies with opam-monorepo, which needs the dune-universe
overlay repository; the `make lock` step adds that repository and then removes
it again before opam-monorepo looks for it, so the lock fails with

    These dependencies (possibly transitive) don't use dune as their build
    system: ptime, ocamlfind, mtime, logs, fmt, cmdliner

This is a toolchain interaction, not a problem with the unikernel: no TON code
is reached. A switch already set up for MirageOS builds — `reuna-matrix` has
solo5 0.12.0, ocaml-solo5 1.3.1 and mirage 4.11.1 — is the natural place to
try it, once the crypto and bignum packages are installed there.

Building for `hvt` additionally needs `solo5` and `ocaml-solo5`, which are not
installed in the `ocaml-ton` switch.
