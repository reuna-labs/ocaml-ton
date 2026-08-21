// TL-B vectors: coins, addresses inside cells, state init and messages.
import {
  beginCell, Address, Cell, storeMessage, storeStateInit, contractAddress,
} from '@ton/core';
import fs from 'fs';
import path from 'path';

const h = (c) => c.hash(0).toString('hex');
const cellOf = (f) => { const b = beginCell(); f(b); return b.endCell(); };

const out = { coins: {}, varuint: {}, address: {}, stateInit: {}, message: {} };

// --- coins ----------------------------------------------------------------
const coinValues = [
  '0', '1', '9', '255', '256', '1000000000', '1500000000',
  '18446744073709551615', '1208925819614629174706175', // 2^80-1
  '1329227995784915872903807060280344575',              // 2^120-1
];
for (const v of coinValues) {
  const c = cellOf((b) => b.storeCoins(BigInt(v)));
  out.coins[v] = { hash: h(c), bits: c.bits.length };
}

// --- var uint (length field width 5, i.e. VarUInteger 32) ------------------
for (const v of ['0', '1', '65535', '4294967295']) {
  const c = cellOf((b) => b.storeVarUint(BigInt(v), 5));
  out.varuint[v] = { hash: h(c), bits: c.bits.length };
}

// --- addresses in cells ----------------------------------------------------
const addrs = {
  none: null,
  base: '0:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76',
  master: '-1:34517c7bdf5187c55af4f8b61fdc321588c7ab768dee24b006df29106458d7cf',
  zero: '0:0000000000000000000000000000000000000000000000000000000000000000',
};
for (const [name, raw] of Object.entries(addrs)) {
  const c = cellOf((b) => b.storeAddress(raw === null ? null : Address.parse(raw)));
  out.address[name] = { raw, hash: h(c), bits: c.bits.length };
}

// --- state init ------------------------------------------------------------
const code = cellOf((b) => b.storeUint(0xdeadbeef, 32));
const data = cellOf((b) => b.storeUint(1, 32).storeUint(0x29a9a317, 32));
{
  const si = { code, data };
  const c = cellOf((b) => b.store(storeStateInit(si)));
  out.stateInit.code_and_data = {
    codeHash: h(code), dataHash: h(data),
    hash: h(c), bits: c.bits.length, refs: c.refs.length,
    address0: contractAddress(0, si).toRawString(),
    addressMinus1: contractAddress(-1, si).toRawString(),
  };
}
{
  const c = cellOf((b) => b.store(storeStateInit({})));
  out.stateInit.empty = { hash: h(c), bits: c.bits.length, refs: c.refs.length };
}

// --- messages --------------------------------------------------------------
const src = Address.parse('0:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76');
const dest = Address.parse('-1:34517c7bdf5187c55af4f8b61fdc321588c7ab768dee24b006df29106458d7cf');
const body = cellOf((b) => b.storeUint(0, 32).storeStringTail('hello'));

{
  const msg = {
    info: {
      type: 'internal', ihrDisabled: true, bounce: true, bounced: false,
      src, dest, value: { coins: 1000000000n },
      ihrFee: 0n, forwardFee: 0n, createdLt: 0n, createdAt: 0,
    },
    body,
  };
  const c = cellOf((b) => b.store(storeMessage(msg)));
  out.message.internal = { hash: h(c), bits: c.bits.length, refs: c.refs.length, bodyHash: h(body) };
}
{
  const msg = {
    info: { type: 'external-in', src: null, dest, importFee: 0n },
    body,
  };
  const c = cellOf((b) => b.store(storeMessage(msg)));
  out.message.external_in = { hash: h(c), bits: c.bits.length, refs: c.refs.length };
}
{
  // With state init, which is how a contract gets deployed.
  const msg = {
    info: { type: 'external-in', src: null, dest, importFee: 0n },
    init: { code, data },
    body,
  };
  const c = cellOf((b) => b.store(storeMessage(msg)));
  out.message.external_in_with_init = { hash: h(c), bits: c.bits.length, refs: c.refs.length };
}
{
  // A body large enough to force the reference form rather than inlining.
  const big = cellOf((b) => b.storeBuffer(Buffer.alloc(120, 0xab)));
  const msg = {
    info: {
      type: 'internal', ihrDisabled: true, bounce: false, bounced: false,
      src, dest, value: { coins: 42n },
      ihrFee: 0n, forwardFee: 0n, createdLt: 7n, createdAt: 1700000000,
    },
    body: big,
  };
  const c = cellOf((b) => b.store(storeMessage(msg)));
  out.message.internal_big_body = { hash: h(c), bits: c.bits.length, refs: c.refs.length, bodyHash: h(big) };
}

const root = path.resolve(process.argv[2] ?? '../..');
fs.writeFileSync(path.join(root, 'test/vectors/tlb-expected.json'), JSON.stringify(out, null, 1) + '\n');
console.log('wrote tlb vectors');
