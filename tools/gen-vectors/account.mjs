// Account vectors, read out of the real mainnet states in test/vectors/ton-core.
import { Cell, loadAccount, storeAccount, beginCell } from '@ton/core';
import fs from 'fs';
import path from 'path';

const root = path.resolve(process.argv[2] ?? '../..');
const dir = path.join(root, 'test/vectors/ton-core');
const out = {};

for (const name of ['accountState.txt', 'accountStateTest.txt']) {
  const src = Buffer.from(fs.readFileSync(path.join(dir, name)).toString().trim(), 'base64');
  const cell = Cell.fromBoc(src)[0];
  const s = cell.beginParse();
  const tag = s.loadBit();               // account$1
  const a = loadAccount(s);
  // Re-serialising must reproduce the very cell we parsed.
  const re = beginCell().storeBit(true).store(storeAccount(a)).endCell();
  out[name] = {
    rootHash: cell.hash(0).toString('hex'),
    tag,
    addr: a.addr.toRawString(),
    used: { cells: a.storageStats.used.cells.toString(), bits: a.storageStats.used.bits.toString() },
    storageExtra: a.storageStats.storageExtra ? a.storageStats.storageExtra.dictHash.toString() : null,
    lastPaid: a.storageStats.lastPaid,
    duePayment: a.storageStats.duePayment ? a.storageStats.duePayment.toString() : null,
    lastTransLt: a.storage.lastTransLt.toString(),
    balance: a.storage.balance.coins.toString(),
    stateType: a.storage.state.type,
    codeHash: a.storage.state.type === 'active' && a.storage.state.state.code
      ? a.storage.state.state.code.hash(0).toString('hex') : null,
    dataHash: a.storage.state.type === 'active' && a.storage.state.state.data
      ? a.storage.state.state.data.hash(0).toString('hex') : null,
    reserializedHash: re.hash(0).toString('hex'),
    reserializedMatchesRoot: re.hash(0).equals(cell.hash(0)),
  };
}

fs.writeFileSync(path.join(root, 'test/vectors/account-expected.json'), JSON.stringify(out, null, 1) + '\n');
console.log(JSON.stringify(out, null, 1).slice(0, 900));
