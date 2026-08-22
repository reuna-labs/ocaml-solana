import {
  AccountRole,
  address,
  appendTransactionMessageInstructions,
  blockhash,
  compileTransactionMessage,
  createTransactionMessage,
  getAddressEncoder,
  getCompiledTransactionMessageEncoder,
  getProgramDerivedAddress,
  pipe,
  setTransactionMessageFeePayer,
  setTransactionMessageLifetimeUsingBlockhash,
} from '@solana/kit';

const OWNER = address('9fYLFVoVqwH37C3dyPi6cpeobfbQ2jtLpN5HgAYDDdkm');
const RECIPIENT = address('AxZfZWeqztBCL37Mkjkd4b8Hf6J13WCcfozrBY6vZzv3');
const MINT = address('EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v');
const TOKEN = address('TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA');
const ASSOCIATED_TOKEN = address('ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL');
const SYSTEM = address('11111111111111111111111111111111');
const LOOKUP_TABLE = address('4uhcVJyU9pJkvQyS88uRDiswHXSCkY3zQawwpjk2NsNY');
const BLOCKHASH = blockhash('4uhcVJyU9pJkvQyS88uRDiswHXSCkY3zQawwpjk2NsNY');
const addressEncoder = getAddressEncoder();

async function ata(owner) {
  return getProgramDerivedAddress({
    programAddress: ASSOCIATED_TOKEN,
    seeds: [
      addressEncoder.encode(owner),
      addressEncoder.encode(TOKEN),
      addressEncoder.encode(MINT),
    ],
  });
}

function transferCheckedData(amount, decimals) {
  const data = new Uint8Array(10);
  data[0] = 12;
  new DataView(data.buffer).setBigUint64(1, amount, true);
  data[9] = decimals;
  return data;
}

function message(version, instructions) {
  return pipe(
    createTransactionMessage({ version }),
    message => setTransactionMessageFeePayer(OWNER, message),
    message =>
      setTransactionMessageLifetimeUsingBlockhash(
        { blockhash: BLOCKHASH, lastValidBlockHeight: 0n },
        message,
      ),
    message => appendTransactionMessageInstructions(instructions, message),
    compileTransactionMessage,
    compiled => getCompiledTransactionMessageEncoder().encode(compiled),
  );
}

function encoded(bytes) {
  return {
    base64: Buffer.from(bytes).toString('base64'),
    hex: Buffer.from(bytes).toString('hex'),
  };
}

const [source, sourceBump] = await ata(OWNER);
const [destination, destinationBump] = await ata(RECIPIENT);

const createDestinationAta = {
  programAddress: ASSOCIATED_TOKEN,
  accounts: [
    { address: OWNER, role: AccountRole.WRITABLE_SIGNER },
    { address: destination, role: AccountRole.WRITABLE },
    { address: RECIPIENT, role: AccountRole.READONLY },
    { address: MINT, role: AccountRole.READONLY },
    { address: SYSTEM, role: AccountRole.READONLY },
    { address: TOKEN, role: AccountRole.READONLY },
  ],
  data: new Uint8Array([1]),
};

const transfer = {
  programAddress: TOKEN,
  accounts: [
    { address: source, role: AccountRole.WRITABLE },
    { address: MINT, role: AccountRole.READONLY },
    { address: destination, role: AccountRole.WRITABLE },
    { address: OWNER, role: AccountRole.READONLY_SIGNER },
  ],
  data: transferCheckedData(1_234_567n, 6),
};

const lookupTransfer = {
  ...transfer,
  accounts: [
    { address: source, role: AccountRole.WRITABLE },
    {
      address: MINT,
      addressIndex: 0,
      lookupTableAddress: LOOKUP_TABLE,
      role: AccountRole.READONLY,
    },
    {
      address: destination,
      addressIndex: 1,
      lookupTableAddress: LOOKUP_TABLE,
      role: AccountRole.WRITABLE,
    },
    { address: OWNER, role: AccountRole.READONLY_SIGNER },
  ],
};

const output = {
  provenance: {
    kit: '8.0.0',
    generator: 'conformance/kit/generate.mjs',
  },
  addresses: {
    owner: OWNER,
    recipient: RECIPIENT,
    mint: MINT,
    sourceAta: source,
    sourceBump,
    destinationAta: destination,
    destinationBump,
    lookupTable: LOOKUP_TABLE,
  },
  legacyAtaAndTransferChecked: encoded(message('legacy', [createDestinationAta, transfer])),
  v0LookupTransferChecked: encoded(message(0, [lookupTransfer])),
};

console.log(JSON.stringify(output, null, 2));
