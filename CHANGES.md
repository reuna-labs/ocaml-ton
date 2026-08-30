# Changes

## 0.1.0~alpha2 (2026-08-30)

- Assigned every hermetic test and generated-schema check to its owning opam
  package, so all 13 subpackages install independently in dependency order.
- Made the OCaml 4.14 build warning-clean and replaced an unobserved Lwt reader
  promise field with `Lwt.async`.
- Validated a clean OCaml 4.14.2 source-package install of the complete stack.

## 0.1.0~alpha1 (2026-08-30)

First public alpha of the pure OCaml TON stack.

- Canonical cells, exotic cells, Bag-of-Cells, TL-B data, addresses and wallet
  contracts v3R2, v4R2 and v5R1.
- Ed25519 keys, TON mnemonics and Ed25519-to-X25519 conversion.
- Generated TL bindings, a sans-I/O ADNL session, typed liteserver queries and
  Unix/Mirage transports.
- Account, shard-link and block-proof verification rooted in a configured
  trusted anchor.
- Hermetic cross-implementation fixtures and MirageOS/Solo5 build validation.
