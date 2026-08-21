// TL constructor identifiers, derived independently.
//
// The identifier is a CRC-32 over the definition with its incidental syntax
// removed. This reimplements that from the written rule and computes it for
// every definition in the vendored schemas, so the OCaml side is checked
// against separate code rather than against itself.
import fs from 'fs';
import path from 'path';
import zlib from 'zlib';

const root = path.resolve(process.argv[2] ?? '../..');

const stripComment = (s) => { const i = s.indexOf('//'); return i < 0 ? s : s.slice(0, i); };

// A pinned #id overrides the computed value; strip it before hashing but
// remember it.
function explicitId(s) {
  const m = stripComment(s).match(/^([A-Za-z][A-Za-z0-9_.]*)#([0-9a-fA-F]{8})(?=[\s=])/);
  return m ? { name: m[1], id: parseInt(m[2], 16) >>> 0 } : null;
}
function stripExplicit(s) {
  return stripComment(s).replace(/^([A-Za-z][A-Za-z0-9_.]*)#[0-9a-fA-F]{8}(?=[\s=])/, '$1');
}
const normalize = (s) => stripExplicit(s).replace(/[();]/g, '').trim().replace(/\s+/g, ' ');
const crc32 = (s) => zlib.crc32(Buffer.from(s, 'utf8')) >>> 0;

// Definitions run to a semicolon; three of them wrap across lines.
function statements(file) {
  const out = [];
  let buf = '';
  for (const raw of fs.readFileSync(file, 'utf8').split('\n')) {
    const line = stripComment(raw);
    const t = line.trim();
    if (t === '---functions---' || t === '---types---' || t === '') continue;
    buf += line + ' ';
    if (t.includes(';')) { out.push(buf.trim()); buf = ''; }
  }
  return out;
}

const BUILTINS = new Set(['int','long','double','string','object','function','bytes','true','boolTrue','boolFalse','vector','int128','int256']);
const out = {};
for (const f of ['lite_api.tl', 'ton_api.tl']) {
  const defs = {};
  for (const src of statements(path.join(root, 'schema', f))) {
    const name = src.trim().split(/[\s#=]/)[0];
    if (BUILTINS.has(name)) continue;
    const ex = explicitId(src);
    defs[name] = {
      // The source line is recorded so the OCaml side can be checked without
      // reimplementing the schema splitter, which would defeat the purpose.
      source: src,
      id: ex ? ex.id : crc32(normalize(src)),
      explicit: !!ex,
    };
  }
  out[f] = defs;
}

fs.writeFileSync(path.join(root, 'test/vectors/tl-expected.json'), JSON.stringify(out, null, 1) + '\n');
const n = Object.values(out).reduce((a, d) => a + Object.keys(d).length, 0);
console.log(`wrote ids for ${n} definitions`);
for (const k of ['liteServer.getMasterchainInfo','liteServer.query','adnl.message.query','liteServer.transactionId'])
  if (out['lite_api.tl'][k]) console.log(`  ${k.padEnd(34)} ${out['lite_api.tl'][k].id.toString(16).padStart(8,'0')}${out['lite_api.tl'][k].explicit?' (pinned)':''}`);
