use serde_json::json;
use solana_message::{
    v0, AccountMeta, Address, AddressLookupTableAccount, Hash, Instruction, Message,
};
use std::str::FromStr;

fn address(value: &str) -> Address {
    Address::from_str(value).unwrap()
}

fn hex(bytes: &[u8]) -> String {
    const DIGITS: &[u8; 16] = b"0123456789abcdef";
    let mut output = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        output.push(DIGITS[(byte >> 4) as usize] as char);
        output.push(DIGITS[(byte & 0x0f) as usize] as char);
    }
    output
}

fn transfer_checked(
    token: Address,
    source: Address,
    mint: Address,
    destination: Address,
    authority: Address,
) -> Instruction {
    let mut data = vec![12];
    data.extend_from_slice(&1_234_567_u64.to_le_bytes());
    data.push(6);
    Instruction {
        program_id: token,
        accounts: vec![
            AccountMeta::new(source, false),
            AccountMeta::new_readonly(mint, false),
            AccountMeta::new(destination, false),
            AccountMeta::new_readonly(authority, true),
        ],
        data,
    }
}

fn main() {
    let owner = address("9fYLFVoVqwH37C3dyPi6cpeobfbQ2jtLpN5HgAYDDdkm");
    let recipient = address("AxZfZWeqztBCL37Mkjkd4b8Hf6J13WCcfozrBY6vZzv3");
    let mint = address("EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v");
    let source = address("5DMGH4bkaN4ZYBHrF7DVaNo3bkp5QagP1tfVUjPu7UAY");
    let destination = address("CpjYkGi3bqqTPYLq8uvSGHTMJQgx3BVJCgCia97rbAsU");
    let token = address("TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA");
    let associated = address("ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL");
    let system = address("11111111111111111111111111111111");
    let lookup = address("4uhcVJyU9pJkvQyS88uRDiswHXSCkY3zQawwpjk2NsNY");
    let blockhash = Hash::from_str("4uhcVJyU9pJkvQyS88uRDiswHXSCkY3zQawwpjk2NsNY").unwrap();

    let create = Instruction {
        program_id: associated,
        accounts: vec![
            AccountMeta::new(owner, true),
            AccountMeta::new(destination, false),
            AccountMeta::new_readonly(recipient, false),
            AccountMeta::new_readonly(mint, false),
            AccountMeta::new_readonly(system, false),
            AccountMeta::new_readonly(token, false),
        ],
        data: vec![1],
    };
    let transfer = transfer_checked(token, source, mint, destination, owner);
    let legacy = Message::new_with_blockhash(&[create, transfer.clone()], Some(&owner), &blockhash)
        .serialize();
    let v0 = v0::Message::try_compile(
        &owner,
        &[transfer],
        &[AddressLookupTableAccount {
            key: lookup,
            addresses: vec![mint, destination],
        }],
        blockhash,
    )
    .unwrap()
    .serialize();

    println!(
        "{}",
        serde_json::to_string_pretty(&json!({
            "provenance": {
                "agave": "4.2.1",
                "solana-message": "4.2.3",
                "generator": "conformance/agave/src/main.rs"
            },
            "legacyAtaAndTransferCheckedHex": hex(&legacy),
            "v0LookupTransferCheckedHex": hex(&v0)
        }))
        .unwrap()
    );
}
