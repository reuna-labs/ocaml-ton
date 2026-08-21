// Address vectors: every combination of workchain, bounceable, testnet and
// base64 alphabet, plus the parse direction, taken from @ton/core.
import { Address } from '@ton/core';
import fs from 'fs';
import path from 'path';

const raws = [
  '0:0000000000000000000000000000000000000000000000000000000000000000',
  '0:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff',
  '0:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76',
  '-1:3333333333333333333333333333333333333333333333333333333333333333',
  '-1:34517c7bdf5187c55af4f8b61fdc321588c7ab768dee24b006df29106458d7cf',
  '0:8d8dc2fbf5a1eaea6bb8b3c0b0e58dfaa42bfbaf7a97e83a3ee3b1e15d3e9e35',
];

const out = {};
for (const raw of raws) {
  const a = Address.parse(raw);
  const forms = {};
  for (const bounceable of [true, false])
    for (const testOnly of [true, false])
      for (const urlSafe of [true, false])
        forms[`bounceable=${bounceable},testnet=${testOnly},urlSafe=${urlSafe}`] =
          a.toString({ urlSafe, bounceable, testOnly });
  out[raw] = {
    workchain: a.workChain,
    hash: a.hash.toString('hex'),
    rawString: a.toRawString(),
    forms,
  };
}

const root = path.resolve(process.argv[2] ?? '../..');
fs.writeFileSync(
  path.join(root, 'test/vectors/address-expected.json'),
  JSON.stringify(out, null, 1) + '\n'
);
console.log(`wrote ${Object.keys(out).length} addresses x ${Object.keys(out[raws[0]].forms).length} forms`);
