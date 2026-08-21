// TVM stack vectors. The integer tag depends on magnitude and the stack is a
// cons list threaded backwards through references, so these cases deliberately
// straddle the int64 boundary and vary the tuple arity.
import { serializeTuple, beginCell, Cell } from '@ton/core';
import fs from 'fs';
import path from 'path';

const cellA = beginCell().storeUint(0xdeadbeef, 32).endCell();
const cellB = beginCell().storeUint(1, 8).storeRef(cellA).endCell();

const cases = {
  empty: [],
  null_only: [{ type: 'null' }],
  small_int: [{ type: 'int', value: 42n }],
  negative_int: [{ type: 'int', value: -42n }],
  int64_max: [{ type: 'int', value: 9223372036854775807n }],
  int64_min: [{ type: 'int', value: -9223372036854775808n }],
  // One past the int64 boundary in each direction: the tag must change here.
  just_over_max: [{ type: 'int', value: 9223372036854775808n }],
  just_under_min: [{ type: 'int', value: -9223372036854775809n }],
  int257_max: [{ type: 'int', value: (1n << 256n) - 1n }],
  int257_min: [{ type: 'int', value: -(1n << 256n) }],
  nan: [{ type: 'nan' }],
  cell: [{ type: 'cell', cell: cellA }],
  builder: [{ type: 'builder', cell: cellA }],
  slice: [{ type: 'slice', cell: cellA }],
  slice_with_refs: [{ type: 'slice', cell: cellB }],
  mixed: [{ type: 'int', value: 1n }, { type: 'null' }, { type: 'cell', cell: cellA }],
  deep: Array.from({ length: 20 }, (_, i) => ({ type: 'int', value: BigInt(i) })),
  tuple_empty: [{ type: 'tuple', items: [] }],
  tuple_one: [{ type: 'tuple', items: [{ type: 'int', value: 7n }] }],
  tuple_two: [{ type: 'tuple', items: [{ type: 'int', value: 1n }, { type: 'int', value: 2n }] }],
  tuple_three: [{ type: 'tuple', items: [{ type: 'int', value: 1n }, { type: 'int', value: 2n }, { type: 'null' }] }],
  tuple_seven: [{ type: 'tuple', items: Array.from({ length: 7 }, (_, i) => ({ type: 'int', value: BigInt(i * 100) })) }],
  tuple_nested: [{ type: 'tuple', items: [
    { type: 'int', value: 1n },
    { type: 'tuple', items: [{ type: 'int', value: 2n }, { type: 'int', value: 3n }] },
    { type: 'cell', cell: cellA },
  ] }],
};

const out = {};
for (const [name, stack] of Object.entries(cases)) {
  const c = serializeTuple(stack);
  out[name] = {
    hash: c.hash(0).toString('hex'),
    bits: c.bits.length,
    refs: c.refs.length,
    boc: c.toBoc({ idx: false, crc32: false }).toString('base64'),
  };
}

const root = path.resolve(process.argv[2] ?? '../..');
fs.writeFileSync(path.join(root, 'test/vectors/vmstack-expected.json'), JSON.stringify(out, null, 1) + '\n');
console.log(`wrote ${Object.keys(out).length} stack cases`);
