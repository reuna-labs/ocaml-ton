// Ed25519 -> X25519 vectors.
//
// The map is computed here from first principles with BigInt arithmetic, and
// the resulting keys are then used for a real X25519 exchange through Node's
// own crypto. So the OCaml side is checked against an implementation that
// shares no code with it, and against the property that actually matters:
// two parties holding Ed25519 signing keys must derive the same secret.
import crypto from 'crypto';
import fs from 'fs';
import path from 'path';

const P = (1n << 255n) - 19n;
const modinv = (a, m) => {
  let [old_r, r] = [((a % m) + m) % m, m];
  let [old_s, s] = [1n, 0n];
  while (r !== 0n) { const q = old_r / r; [old_r, r] = [r, old_r - q * r]; [old_s, s] = [s, old_s - q * s]; }
  return ((old_s % m) + m) % m;
};
const leToBig = (b) => { let v = 0n; for (let i = b.length - 1; i >= 0; i--) v = (v << 8n) | BigInt(b[i]); return v; };
const bigToLe = (v) => { const b = Buffer.alloc(32); for (let i = 0; i < 32; i++) { b[i] = Number(v & 0xffn); v >>= 8n; } return b; };

// u = (1 + y) / (1 - y) mod p, with the sign bit of the encoding discarded.
function edPubToMontU(pub) {
  const t = Buffer.from(pub); t[31] &= 0x7f;
  const y = leToBig(t) % P;
  return bigToLe(((1n + y) * modinv(((1n - y) % P + P) % P, P)) % P);
}

const ED_PRIV_DER = Buffer.from('302e020100300506032b657004220420', 'hex');
const X_PRIV_DER = Buffer.from('302e020100300506032b656e04220420', 'hex');
const X_PUB_DER = Buffer.from('302a300506032b656e032100', 'hex');

const edPub = (seed) => {
  const k = crypto.createPrivateKey({ key: Buffer.concat([ED_PRIV_DER, seed]), format: 'der', type: 'pkcs8' });
  return crypto.createPublicKey(k).export({ format: 'der', type: 'spki' }).subarray(12);
};
// The X25519 scalar is the clamped low half of SHA-512 of the seed.
const xScalar = (seed) => {
  const h = crypto.createHash('sha512').update(seed).digest();
  const s = h.subarray(0, 32);
  s[0] &= 248; s[31] &= 127; s[31] |= 64;
  return Buffer.from(s);
};
const dh = (scalar, peerU) => crypto.diffieHellman({
  privateKey: crypto.createPrivateKey({ key: Buffer.concat([X_PRIV_DER, scalar]), format: 'der', type: 'pkcs8' }),
  publicKey: crypto.createPublicKey({ key: Buffer.concat([X_PUB_DER, peerU]), format: 'der', type: 'spki' }),
});

const out = { cases: [], agreements: [] };
const seeds = [
  '00'.repeat(32), '01'.repeat(32), 'ff'.repeat(32),
  '9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60',
  '4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb',
  'c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7',
];
for (const hexSeed of seeds) {
  const seed = Buffer.from(hexSeed, 'hex');
  const pub = edPub(seed);
  out.cases.push({
    seed: hexSeed,
    edPub: pub.toString('hex'),
    xScalar: xScalar(seed).toString('hex'),
    xPub: edPubToMontU(pub).toString('hex'),
  });
}
// Both directions of an exchange must land on the same secret.
for (let i = 0; i + 1 < seeds.length; i += 2) {
  const a = Buffer.from(seeds[i], 'hex'), b = Buffer.from(seeds[i + 1], 'hex');
  const sa = dh(xScalar(a), edPubToMontU(edPub(b)));
  const sb = dh(xScalar(b), edPubToMontU(edPub(a)));
  if (!sa.equals(sb)) throw new Error('reference disagrees with itself');
  out.agreements.push({ seedA: seeds[i], seedB: seeds[i + 1], shared: sa.toString('hex') });
}
// y = 1 encodes the identity and has no Montgomery image.
out.identity = bigToLe(1n).toString('hex');

const root = path.resolve(process.argv[2] ?? '../..');
fs.writeFileSync(path.join(root, 'test/vectors/x25519-expected.json'), JSON.stringify(out, null, 1) + '\n');
console.log(`cases=${out.cases.length} agreements=${out.agreements.length}`);
console.log('sample xPub:', out.cases[3].xPub);
console.log('shared[0]  :', out.agreements[0].shared);
