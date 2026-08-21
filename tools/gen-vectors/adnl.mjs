// ADNL handshake vectors, built independently from the protocol description.
//
// Everything random is fixed here, so the packet is a pure function of its
// inputs and can be compared byte for byte. This is the only offline check
// on the handshake layout: a wrong one does not produce an error, it produces
// a server that says nothing.
import crypto from 'crypto';
import fs from 'fs';
import path from 'path';

const P = (1n << 255n) - 19n;
const modinv = (a, m) => { let [r0,r1]=[((a%m)+m)%m,m],[s0,s1]=[1n,0n];
  while (r1 !== 0n) { const q=r0/r1; [r0,r1]=[r1,r0-q*r1]; [s0,s1]=[s1,s0-q*s1]; } return ((s0%m)+m)%m; };
const leToBig = (b) => { let v=0n; for (let i=b.length-1;i>=0;i--) v=(v<<8n)|BigInt(b[i]); return v; };
const bigToLe = (v) => { const b=Buffer.alloc(32); for (let i=0;i<32;i++){b[i]=Number(v&0xffn); v>>=8n;} return b; };
const edPubToU = (pub) => { const t=Buffer.from(pub); t[31]&=0x7f; const y=leToBig(t)%P;
  return bigToLe(((1n+y)*modinv(((1n-y)%P+P)%P,P))%P); };

const ED_DER = Buffer.from('302e020100300506032b657004220420','hex');
const X_PRIV_DER = Buffer.from('302e020100300506032b656e04220420','hex');
const X_PUB_DER = Buffer.from('302a300506032b656e032100','hex');
const edPub = (seed) => crypto.createPublicKey(
  crypto.createPrivateKey({key:Buffer.concat([ED_DER,seed]),format:'der',type:'pkcs8'})
).export({format:'der',type:'spki'}).subarray(12);
const xScalar = (seed) => { const h=crypto.createHash('sha512').update(seed).digest();
  const s=Buffer.from(h.subarray(0,32)); s[0]&=248; s[31]&=127; s[31]|=64; return s; };
const dh = (scalar, peerU) => crypto.diffieHellman({
  privateKey: crypto.createPrivateKey({key:Buffer.concat([X_PRIV_DER,scalar]),format:'der',type:'pkcs8'}),
  publicKey: crypto.createPublicKey({key:Buffer.concat([X_PUB_DER,peerU]),format:'der',type:'spki'})});
const sha256 = (b) => crypto.createHash('sha256').update(b).digest();
const ctr = (key, iv, data) => { const c = crypto.createCipheriv('aes-256-ctr', key, iv);
  return Buffer.concat([c.update(data), c.final()]); };

// The identifier hashes the TL-boxed key: the pub.ed25519 constructor id,
// little-endian, followed by the key.
const keyId = (pub) => sha256(Buffer.concat([Buffer.from('c6b41348','hex'), pub]));

function handshake(serverPub, ephemeralSeed, aesParams) {
  const secret = dh(xScalar(ephemeralSeed), edPubToU(serverPub));
  const checksum = sha256(aesParams);
  const key = Buffer.concat([secret.subarray(0,16), checksum.subarray(16,32)]);
  const iv  = Buffer.concat([checksum.subarray(0,4), secret.subarray(20,32)]);
  const packet = Buffer.concat([keyId(serverPub), edPub(ephemeralSeed), checksum, ctr(key, iv, aesParams)]);
  return {
    secret: secret.toString('hex'),
    checksum: checksum.toString('hex'),
    handshakeKey: key.toString('hex'),
    handshakeIv: iv.toString('hex'),
    packet: packet.toString('hex'),
    rxKey: aesParams.subarray(0,32).toString('hex'),
    txKey: aesParams.subarray(32,64).toString('hex'),
    rxIv: aesParams.subarray(64,80).toString('hex'),
    txIv: aesParams.subarray(80,96).toString('hex'),
  };
}

// A frame, encrypted with the transmit stream.
function frame(txKey, txIv, nonce, payload) {
  const body = Buffer.concat([nonce, payload]);
  const size = Buffer.alloc(4); size.writeUInt32LE(64 + payload.length);
  const plain = Buffer.concat([size, body, sha256(body)]);
  return { plain: plain.toString('hex'), encrypted: ctr(txKey, txIv, plain).toString('hex') };
}

const out = { keyIds: [], handshakes: [], frames: [] };

for (const h of ['00'.repeat(32), '11'.repeat(32),
                 'n4VDnSCUuSpjnCyUk9e3QOOd6o0ItSWYbTnW3Wnn8wk=']) {
  const pub = h.length === 64 ? Buffer.from(h, 'hex') : Buffer.from(h, 'base64');
  out.keyIds.push({ pub: pub.toString('hex'), id: keyId(pub).toString('hex') });
}

for (const [serverHex, seedHex, paramsSeed] of [
  ['n4VDnSCUuSpjnCyUk9e3QOOd6o0ItSWYbTnW3Wnn8wk=', '01'.repeat(32), 'a1'],
  ['11'.repeat(32), '9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', '5c'],
]) {
  const serverPub = serverHex.length === 64 ? Buffer.from(serverHex,'hex') : Buffer.from(serverHex,'base64');
  const seed = Buffer.from(seedHex, 'hex');
  // Deterministic parameters: a repeating byte pattern rather than randomness.
  const params = Buffer.from(Array.from({length:160},(_,i)=>(parseInt(paramsSeed,16)+i)&0xff));
  out.handshakes.push({
    serverPub: serverPub.toString('hex'),
    ephemeralSeed: seedHex,
    aesParams: params.toString('hex'),
    ...handshake(serverPub, seed, params),
  });
}

{
  const h = out.handshakes[0];
  const txKey = Buffer.from(h.txKey,'hex'), txIv = Buffer.from(h.txIv,'hex');
  for (const [nonceByte, payloadHex] of [['22',''], ['33','deadbeef'], ['44','ff'.repeat(200)]]) {
    const nonce = Buffer.alloc(32, parseInt(nonceByte,16));
    const payload = Buffer.from(payloadHex,'hex');
    out.frames.push({ nonce: nonce.toString('hex'), payload: payloadHex, ...frame(txKey, txIv, nonce, payload) });
  }
  // Two frames back to back share one keystream, which is where a naive
  // implementation restarts the counter and desynchronises.
  const c = crypto.createCipheriv('aes-256-ctr', txKey, txIv);
  const mk = (n, p) => { const body=Buffer.concat([n,p]); const s=Buffer.alloc(4); s.writeUInt32LE(64+p.length);
    return Buffer.concat([s, body, sha256(body)]); };
  const a = mk(Buffer.alloc(32,0x55), Buffer.from('0102','hex'));
  const b = mk(Buffer.alloc(32,0x66), Buffer.from('030405','hex'));
  out.consecutive = {
    nonces: ['55'.repeat(32), '66'.repeat(32)],
    payloads: ['0102', '030405'],
    encrypted: Buffer.concat([c.update(a), c.update(b), c.final()]).toString('hex'),
  };
}

const root = path.resolve(process.argv[2] ?? '../..');
fs.writeFileSync(path.join(root, 'test/vectors/adnl-expected.json'), JSON.stringify(out, null, 1) + '\n');
console.log(`keyIds=${out.keyIds.length} handshakes=${out.handshakes.length} frames=${out.frames.length}`);
console.log('packet[0][0..64]:', out.handshakes[0].packet.slice(0, 64));
