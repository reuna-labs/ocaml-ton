# gen-vectors

Regenerates `test/vectors/ton-core-expected.json` by running the reference
TypeScript SDK over the committed Bag-of-Cells fixtures.

```sh
npm install
node generate.mjs
```

The generated JSON is committed on purpose: the OCaml test suite must run
offline, with no Node and no network. Re-run this only when adding fixtures or
when deliberately moving to a new `@ton/core` version.
