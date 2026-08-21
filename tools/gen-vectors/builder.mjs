// Builder/Slice vectors. Each case is a small program of store operations,
// interpreted by both this script and the OCaml test suite, so the two are
// running the same spec rather than two hand-written transcriptions.
import { beginCell, Cell } from '@ton/core';
import fs from 'fs';
import path from 'path';
import crypto from 'crypto';

const sha256 = (b) => crypto.createHash('sha256').update(b).digest('hex');

// op := ["uint"|"int", decimalString, bits] | ["bit", bool] | ["bytes", hex]
//     | ["ref", ops] | ["maybe_ref", ops|null]
const cases = {
  empty: [],
  single_bit: [['bit', true]],
  two_bits: [['bit', false], ['bit', true]],
  uint8: [['uint', '171', 8]],
  uint32: [['uint', '3735928559', 32]],
  uint64_max: [['uint', '18446744073709551615', 64]],
  uint_zero_width: [['uint', '0', 0], ['uint', '1', 1]],
  int8_neg: [['int', '-1', 8]],
  int8_min: [['int', '-128', 8]],
  int16_neg: [['int', '-12345', 16]],
  int257_neg: [['int', '-57896044618658097711785492504343953926634992332820282019728792003956564819968', 257]],
  uint256: [['uint', '115792089237316195423570985008687907853269984665640564039457584007913129639935', 256]],
  // Unaligned widths are where the completion tag starts mattering.
  odd_widths: [['uint', '5', 3], ['uint', '9', 5], ['uint', '1', 1], ['uint', '255', 8]],
  bytes: [['bytes', 'deadbeefcafe']],
  full_1023: [['bytes', 'ff'.repeat(127)], ['uint', '127', 7]],
  one_ref: [['ref', [['uint', '1', 8]]]],
  four_refs: [
    ['ref', [['uint', '1', 8]]], ['ref', [['uint', '2', 8]]],
    ['ref', [['uint', '3', 8]]], ['ref', [['uint', '4', 8]]],
  ],
  nested_refs: [['ref', [['uint', '1', 8], ['ref', [['uint', '2', 8], ['ref', [['uint', '3', 8]]]]]]]],
  maybe_none: [['maybe_ref', null]],
  maybe_some: [['maybe_ref', [['uint', '42', 8]]]],
  mixed: [
    ['uint', '2', 2], ['bit', true],
    ['uint', '1000000000', 64],
    ['ref', [['bytes', '0011223344556677'], ['bit', false]]],
    ['uint', '7', 3],
  ],
};

function build(ops) {
  const b = beginCell();
  for (const op of ops) {
    switch (op[0]) {
      case 'uint': b.storeUint(BigInt(op[1]), op[2]); break;
      case 'int': b.storeInt(BigInt(op[1]), op[2]); break;
      case 'bit': b.storeBit(op[1]); break;
      case 'bytes': b.storeBuffer(Buffer.from(op[1], 'hex')); break;
      case 'ref': b.storeRef(build(op[1])); break;
      case 'maybe_ref': b.storeMaybeRef(op[1] === null ? null : build(op[1])); break;
      default: throw new Error(`unknown op ${op[0]}`);
    }
  }
  return b.endCell();
}

const out = {};
for (const [name, ops] of Object.entries(cases)) {
  const c = build(ops);
  const boc = c.toBoc({ idx: false, crc32: false });
  out[name] = {
    ops,
    bits: c.bits.length,
    refs: c.refs.length,
    hash0: c.hash(0).toString('hex'),
    depth0: c.depth(0),
    boc_len: boc.length,
    boc_sha256: sha256(boc),
    boc: boc.toString('base64'),
  };
}

const root = path.resolve(process.argv[2] ?? '../..');
fs.writeFileSync(
  path.join(root, 'test/vectors/builder-expected.json'),
  JSON.stringify(out, null, 1) + '\n'
);
console.log(`wrote ${Object.keys(out).length} builder cases`);
