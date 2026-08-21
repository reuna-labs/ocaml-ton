// Developer tool. Generates cross-implementation test vectors by running the
// reference TypeScript SDK (@ton/core) over the committed fixtures and
// recording what it produces. The output JSON is committed, so the OCaml test
// suite stays offline and needs neither Node nor a network.
//
//   npm install && node generate.mjs
//
// Note: the .txt fixtures were produced by the C++ node and @ton/core does not
// re-serialize them byte-for-byte either, so "expected bytes" here means "what
// @ton/core emits", which is what we are matching against.
import { Cell } from '@ton/core';
import fs from 'fs';
import path from 'path';
import crypto from 'crypto';

const sha256 = (b) => crypto.createHash('sha256').update(b).digest('hex');

const root = path.resolve(process.argv[2] ?? '../..');
const dir = path.join(root, 'test/vectors/ton-core');
const out = {};

for (const name of fs.readdirSync(dir).sort()) {
  if (!name.endsWith('.txt') && !name.endsWith('.boc')) continue;
  const raw = fs.readFileSync(path.join(dir, name));
  const src = raw.subarray(0, 3).toString() === 'te6'
    ? Buffer.from(raw.toString().trim(), 'base64')
    : raw;
  const roots = Cell.fromBoc(src);
  const f = src.readUInt8(4);
  const entry = {
    flags: { idx: !!(f & 0x80), crc32: !!(f & 0x40), cache: !!(f & 0x20) },
    roots: roots.map((c) => ({
      hash0: c.hash(0).toString('hex'),
      hash3: c.hash(3).toString('hex'),
      depth0: c.depth(0),
      level: c.level(),
      cells: (function count(c, seen = new Set()) {
        const k = c.hash(3).toString('hex');
        if (seen.has(k)) return seen.size;
        seen.add(k);
        for (const r of c.refs) count(r, seen);
        return seen.size;
      })(c),
    })),
  };
  if (roots.length === 1) {
    // Digest rather than the bytes: storing four full re-serializations of
    // every fixture would add ~1 MB of base64 to the repository, and a digest
    // mismatch is just as decisive. Re-run this tool to recover the bytes.
    entry.reserialized = {};
    for (const idx of [false, true])
      for (const crc32 of [false, true]) {
        const b = roots[0].toBoc({ idx, crc32 });
        entry.reserialized[`idx=${idx},crc32=${crc32}`] = { len: b.length, sha256: sha256(b) };
      }
  }
  out[name] = entry;
}

fs.writeFileSync(
  path.join(root, 'test/vectors/ton-core-expected.json'),
  JSON.stringify(out, null, 1) + '\n'
);
console.log(`wrote ${Object.keys(out).length} entries`);
