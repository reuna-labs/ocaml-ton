// Wallet vectors: code cells, deploy addresses and signed transfer bodies for
// v3R2, v4R2 and v5R1.
//
// Everything is fixed -- the key comes from a pinned mnemonic and valid_until
// is passed explicitly -- so the signed bytes are reproducible. Without that
// the SDK defaults to "now + 60s" and nothing would be comparable.
import { createRequire } from 'module';
import fs from 'fs';
import path from 'path';
const require = createRequire(import.meta.url);
const { WalletContractV3R2, WalletContractV4, WalletContractV5R1 } = require('@ton/ton');
const { internal, external, beginCell, Address, storeMessageRelaxed, SendMode, toNano } = require('@ton/core');
const { mnemonicToPrivateKey } = require('@ton/crypto');

const root = path.resolve(process.argv[2] ?? '../..');
const crypt = JSON.parse(fs.readFileSync(path.join(root, 'test/vectors/crypto-expected.json')));
const words = crypt.mnemonics[0].words;
const kp = await mnemonicToPrivateKey(words);

const dest = Address.parse('0:e4d954ef9f4e1250a26b5bbad76a1cdd17cfd08babad6f4c23e372270aef6f76');
const dest2 = Address.parse('-1:34517c7bdf5187c55af4f8b61fdc321588c7ab768dee24b006df29106458d7cf');
const VALID_UNTIL = 1893456000; // 2030-01-01, fixed so signatures are stable
const h = (c) => c.hash(0).toString('hex');

const out = { key: { publicKey: kp.publicKey.toString('hex'), mnemonic: words }, wallets: {} };

const variants = [
  ['v3r2_wc0', () => WalletContractV3R2.create({ workchain: 0, publicKey: kp.publicKey })],
  ['v3r2_wc-1', () => WalletContractV3R2.create({ workchain: -1, publicKey: kp.publicKey })],
  ['v4r2_wc0', () => WalletContractV4.create({ workchain: 0, publicKey: kp.publicKey })],
  ['v4r2_wc-1', () => WalletContractV4.create({ workchain: -1, publicKey: kp.publicKey })],
  // v5r1 takes its workchain from the walletId context, not from a separate
  // argument: passing `workchain` alone moves the address but leaves the
  // walletId at its wc0 default, producing a wallet the contract would reject.
  ['v5r1_wc0', () => WalletContractV5R1.create({ publicKey: kp.publicKey,
    walletId: { networkGlobalId: -239, context: { workchain: 0, walletVersion: 'v5r1', subwalletNumber: 0 } } })],
  ['v5r1_wc-1', () => WalletContractV5R1.create({ publicKey: kp.publicKey,
    walletId: { networkGlobalId: -239, context: { workchain: -1, walletVersion: 'v5r1', subwalletNumber: 0 } } })],
  ['v5r1_testnet_wc0', () => WalletContractV5R1.create({ publicKey: kp.publicKey,
    walletId: { networkGlobalId: -3, context: { workchain: 0, walletVersion: 'v5r1', subwalletNumber: 0 } } })],
  ['v5r1_subwallet7', () => WalletContractV5R1.create({ publicKey: kp.publicKey,
    walletId: { networkGlobalId: -239, context: { workchain: 0, walletVersion: 'v5r1', subwalletNumber: 7 } } })],
];

const transfers = {
  single: [internal({ to: dest, value: toNano('1.5'), bounce: true, body: beginCell().storeUint(0, 32).storeStringTail('hi').endCell() })],
  no_body: [internal({ to: dest, value: toNano('0.05'), bounce: false })],
  two: [
    internal({ to: dest, value: toNano('1'), bounce: true }),
    internal({ to: dest2, value: toNano('2'), bounce: false }),
  ],
};

for (const [name, make] of variants) {
  const w = make();
  const entry = {
    workchain: w.address.workChain,
    walletId: (() => { try { return typeof w.walletId === 'object' ? JSON.parse(JSON.stringify(w.walletId, (k,v)=>typeof v==='bigint'?v.toString():v)) : w.walletId; } catch { return null; } })(),
    codeHash: h(w.init.code),
    codeBoc: w.init.code.toBoc({ idx: false, crc32: false }).toString('base64'),
    dataHash: h(w.init.data),
    dataBoc: w.init.data.toBoc({ idx: false, crc32: false }).toString('base64'),
    address: w.address.toRawString(),
    addressFriendly: w.address.toString({ urlSafe: true, bounceable: true, testOnly: false }),
    transfers: {},
  };
  for (const [tname, messages] of Object.entries(transfers)) {
    const body = w.createTransfer({
      seqno: 5, secretKey: kp.secretKey, messages,
      sendMode: SendMode.PAY_GAS_SEPARATELY | SendMode.IGNORE_ERRORS,
      timeout: VALID_UNTIL,
    });
    // seqno 0 also gets a deploy-shaped external with state init attached.
    const ext = external({ to: w.address, init: null, body });
    entry.transfers[tname] = {
      seqno: 5,
      validUntil: VALID_UNTIL,
      sendMode: 3,
      bodyHash: h(body),
      bodyBoc: body.toBoc({ idx: false, crc32: false }).toString('base64'),
      externalHash: h(beginCell().store(require('@ton/core').storeMessage(ext)).endCell()),
    };
  }
  out.wallets[name] = entry;
}

// The relaxed messages themselves, so the OCaml side can be checked on those
// separately from the wallet layer.
out.relaxed = {};
for (const [tname, messages] of Object.entries(transfers)) {
  out.relaxed[tname] = messages.map((m) => h(beginCell().store(storeMessageRelaxed(m)).endCell()));
}

// The serialised wallet_id each wallet actually stores, read back out of its
// own data cell. For v5r1 this is global_id XOR context_id, and these four
// values are the ones every other implementation quotes.
out.walletIds = {};
for (const [name, e] of Object.entries(out.wallets)) {
  const { Cell } = require('@ton/core');
  const c = Cell.fromBoc(Buffer.from(e.dataBoc, 'base64'))[0];
  const s2 = c.beginParse();
  if (name.startsWith('v5')) s2.loadBit();
  s2.loadUint(32);
  out.walletIds[name] = s2.loadUint(32);
}

fs.writeFileSync(path.join(root, 'test/vectors/wallet-expected.json'), JSON.stringify(out, null, 1) + '\n');
console.log('walletIds:', JSON.stringify(out.walletIds));
console.log('wallets:', Object.keys(out.wallets).join(', '));
for (const [n, e] of Object.entries(out.wallets)) console.log(`  ${n.padEnd(10)} ${e.address}  walletId=${JSON.stringify(e.walletId)}`);
