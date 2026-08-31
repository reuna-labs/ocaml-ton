# Security policy

This repository is unaudited alpha software. Do not use it to control assets of
value. Report vulnerabilities privately to `security@reuna.io` rather than
opening a public issue.

## Review boundary

Assume every liteserver and every Bag of Cells is hostile. Review bit/index and
allocation bounds, exotic-cell semantics, TL/TL-B decoding, ADNL session state,
proof-root selection, shard/account proof traversal, wallet message signing and
retention of the exact bytes and hashes being verified.

The current alpha does not provide a rollback-protected persistent verified
head, robust peer rotation or live-network assurance. Proof verification is
useful only when anchored to a trusted and monotonic root. OCaml key material is
not reliably zeroized; valuable keys should remain in an external signer.
