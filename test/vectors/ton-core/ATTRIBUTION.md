# Test vectors

The files in this directory are copied verbatim from
[ton-org/ton-core](https://github.com/ton-org/ton-core), path
`src/boc/cell/__testdata__/`, which is licensed MIT (Copyright (c) Whales Corp.).

They are real mainnet Bags of Cells and are used here as differential test
vectors: our decoder must reproduce the same cell tree and the same
representation hashes as the reference implementation, and re-serialize to the
same bytes.

`accountProof.txt` and `configProof.txt` are the important ones — they contain
Merkle proofs with pruned branches at multiple levels, which is the only way to
exercise the level-mask and higher-hash logic against real data.
