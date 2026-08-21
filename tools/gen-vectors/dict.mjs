// Dictionary vectors. The label encoding picks whichever of hml_short,
// hml_long and hml_same is shortest, and that choice is part of the canonical
// encoding -- disagreeing changes the cell hash. These cases are chosen to
// drive each form, including the ties between them.
import { beginCell, Dictionary } from '@ton/core';
import fs from 'fs';
import path from 'path';
import crypto from 'crypto';

const sha256 = (b) => crypto.createHash('sha256').update(b).digest('hex');

const cases = {
  single_zero:        { keyBits: 8,  valueBits: 8,  keys: [0] },
  single_max:         { keyBits: 8,  valueBits: 8,  keys: [255] },
  // Root label empty, both children all-same: drives hml_same on both sides.
  extremes:           { keyBits: 8,  valueBits: 8,  keys: [0, 255] },
  low_three:          { keyBits: 8,  valueBits: 16, keys: [1, 2, 3] },
  one_bit_key:        { keyBits: 1,  valueBits: 8,  keys: [0, 1] },
  // A complete tree: every internal node forks with an empty label.
  complete_4bit:      { keyBits: 4,  valueBits: 8,  keys: [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] },
  dense_32:           { keyBits: 32, valueBits: 32, keys: [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] },
  // Long shared prefixes, which is where hml_long beats hml_short.
  long_prefix:        { keyBits: 12, valueBits: 8,  keys: [0xabc, 0xabd, 0xabe] },
  sparse_64:          { keyBits: 64, valueBits: 64, keys: ['1', '1000000007', '18446744073709551615', '9223372036854775808'] },
  short_label:        { keyBits: 3,  valueBits: 8,  keys: [5] },
  two_close:          { keyBits: 16, valueBits: 8,  keys: [0x1234, 0x1235] },
  big_keys_256:       { keyBits: 256, valueBits: 8,
                        keys: ['0',
                               '115792089237316195423570985008687907853269984665640564039457584007913129639935',
                               '57896044618658097711785492504343953926634992332820282019728792003956564819968'] },
  // 267 bits is the key width wallet v4 uses for its plugin dictionary.
  wallet_plugins_267: { keyBits: 267, valueBits: 1, keys: ['0', '1', '236943127493075866426519336207415123168358761932822939884085279051776'] },
  scattered:          { keyBits: 10, valueBits: 8,  keys: [0, 1, 512, 513, 1023, 341, 682] },
};

const out = {};
for (const [name, spec] of Object.entries(cases)) {
  const d = Dictionary.empty(
    Dictionary.Keys.BigUint(spec.keyBits),
    Dictionary.Values.BigUint(spec.valueBits)
  );
  const entries = [];
  spec.keys.forEach((k, i) => {
    const key = BigInt(k);
    // Deterministic values that stay inside valueBits.
    const value = BigInt(i * 7 + 1) % (1n << BigInt(spec.valueBits));
    d.set(key, value);
    entries.push([key.toString(), value.toString()]);
  });

  const direct = beginCell().storeDictDirect(d).endCell();
  const wrapped = beginCell().storeDict(d).endCell();
  out[name] = {
    keyBits: spec.keyBits,
    valueBits: spec.valueBits,
    entries,
    direct: { hash: direct.hash(0).toString('hex'), bits: direct.bits.length, refs: direct.refs.length },
    wrapped: { hash: wrapped.hash(0).toString('hex'), boc_sha256: sha256(wrapped.toBoc({ idx: false, crc32: false })) },
    boc: direct.toBoc({ idx: false, crc32: false }).toString('base64'),
  };
}

// The empty dictionary has no Hashmap encoding at all, only the HashmapE bit.
{
  const d = Dictionary.empty(Dictionary.Keys.BigUint(8), Dictionary.Values.BigUint(8));
  const wrapped = beginCell().storeDict(d).endCell();
  out.empty = {
    keyBits: 8, valueBits: 8, entries: [],
    wrapped: { hash: wrapped.hash(0).toString('hex'), boc_sha256: sha256(wrapped.toBoc({ idx: false, crc32: false })) },
  };
}

const root = path.resolve(process.argv[2] ?? '../..');
fs.writeFileSync(path.join(root, 'test/vectors/dict-expected.json'), JSON.stringify(out, null, 1) + '\n');
console.log(`wrote ${Object.keys(out).length} dictionary cases`);
