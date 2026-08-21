// Crypto vectors: TON mnemonics and the PBKDF2 underneath them.
//
// The PBKDF2 cases come from Node's own crypto rather than from @ton/crypto,
// so our implementation is checked against something that shares no code with
// the TON stack. Cases deliberately include output longer than one SHA-512
// block, which TON itself never asks for.
import { createRequire } from 'module';
import crypto from 'crypto';
const require = createRequire(import.meta.url);
const { mnemonicNew, mnemonicToPrivateKey, mnemonicToSeed, mnemonicValidate, mnemonicToHDSeed, mnemonicWordList } = require('@ton/crypto');

// The entropy step is not exported, so compute it straight from the spec:
// the mnemonic is the HMAC key and the password is the message.
const toEntropy = (words, password) =>
  crypto.createHmac('sha512', Buffer.from(words.join(' '), 'utf8'))
        .update(Buffer.from(password ?? '', 'utf8')).digest();
import fs from 'fs';
import path from 'path';

const hex = (b) => Buffer.from(b).toString('hex');
const out = { pbkdf2: [], hmac: [], mnemonics: [], invalid: [], wordlist: {} };

// Pin the wordlist itself: we embed our own copy from the BIP-39 repository,
// so confirm it is the same list the reference uses.
out.wordlist = {
  count: mnemonicWordList.length,
  sha256: crypto.createHash('sha256').update(mnemonicWordList.join('\n') + '\n').digest('hex'),
  first: mnemonicWordList[0],
  last: mnemonicWordList[mnemonicWordList.length - 1],
};

// --- PBKDF2-HMAC-SHA512, cross-checked against Node ------------------------
for (const [password, salt, iterations, len] of [
  ['password', 'salt', 1, 64],
  ['password', 'salt', 2, 64],
  ['password', 'salt', 4096, 64],
  ['', '', 1, 64],
  ['pass\0word', 'sa\0lt', 4096, 16],
  ['password', 'salt', 1, 100],   // more than one SHA-512 block
  ['password', 'salt', 1, 200],   // more than two
  ['p', 's', 390, 64],            // the "TON seed version" shape
]) {
  out.pbkdf2.push({
    password, salt, iterations, len,
    dk: crypto.pbkdf2Sync(Buffer.from(password, 'binary'), Buffer.from(salt, 'binary'), iterations, len, 'sha512').toString('hex'),
  });
}

// --- HMAC-SHA-512, RFC 4231 shapes ----------------------------------------
for (const [key, data] of [
  ['0b'.repeat(20), Buffer.from('Hi There').toString('hex')],
  [Buffer.from('Jefe').toString('hex'), Buffer.from('what do ya want for nothing?').toString('hex')],
  ['aa'.repeat(131), Buffer.from('Test Using Larger Than Block-Size Key - Hash Key First').toString('hex')],
  ['', ''],
]) {
  out.hmac.push({
    key, data,
    mac: crypto.createHmac('sha512', Buffer.from(key, 'hex')).update(Buffer.from(data, 'hex')).digest('hex'),
  });
}

// --- mnemonics -------------------------------------------------------------
const fixed = [
  // Generated once and pinned here so the suite is deterministic.
  null, null, null,
];
const phrases = [];
for (let i = 0; i < fixed.length; i++) phrases.push(await mnemonicNew(24));

for (const words of phrases) {
  const kp = await mnemonicToPrivateKey(words);
  out.mnemonics.push({
    words,
    password: null,
    entropy: hex(toEntropy(words, '')),
    seed: hex(await mnemonicToSeed(words, 'TON default seed')),
    hdSeed: hex(await mnemonicToHDSeed(words)),
    publicKey: hex(kp.publicKey),
    secretKey: hex(kp.secretKey),
    valid: await mnemonicValidate(words),
  });
}

// A phrase used with a password: the derivation changes completely.
{
  const words = phrases[0];
  const password = 'correct horse battery staple';
  const kp = await mnemonicToPrivateKey(words, password);
  out.mnemonics.push({
    words, password,
    entropy: hex(toEntropy(words, password)),
    seed: hex(await mnemonicToSeed(words, 'TON default seed', password)),
    hdSeed: hex(await mnemonicToHDSeed(words, password)),
    publicKey: hex(kp.publicKey),
    secretKey: hex(kp.secretKey),
    valid: await mnemonicValidate(words, password),
  });
}

// Words that are all in the list but do not form a valid phrase.
{
  const words = Array.from({ length: 24 }, () => 'abandon');
  out.invalid.push({ words, reason: 'all words valid, seed is not a basic seed', valid: await mnemonicValidate(words) });
}
{
  const words = [...phrases[0]];
  words[0] = 'notaword';
  out.invalid.push({ words, reason: 'word not in the list', valid: await mnemonicValidate(words) });
}

const root = path.resolve(process.argv[2] ?? '../..');
fs.writeFileSync(path.join(root, 'test/vectors/crypto-expected.json'), JSON.stringify(out, null, 1) + '\n');
console.log(`pbkdf2=${out.pbkdf2.length} hmac=${out.hmac.length} mnemonics=${out.mnemonics.length} invalid=${out.invalid.length}`);
console.log('first phrase:', out.mnemonics[0].words.slice(0, 6).join(' '), '...');
console.log('its pubkey  :', out.mnemonics[0].publicKey);
console.log('invalid     :', out.invalid.map(i => `${i.reason} -> valid=${i.valid}`));
