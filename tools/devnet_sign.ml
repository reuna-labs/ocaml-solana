(* Signs one SOL transfer from a fresh key with the published packages, and
   prints the payer and the wire bytes for a devnet node to judge. The key is
   new and unfunded, so a node that verifies the signature goes on to look the
   payer up and answers AccountNotFound; one byte changed anywhere in the
   signature or the message makes it answer SignatureFailure instead.
   SOLANA_BLOCKHASH is a recent devnet blockhash. *)
let get what = function Ok x -> x | Error _ -> failwith what

let urandom n =
  In_channel.with_open_bin "/dev/urandom" (fun ic -> really_input_string ic n)

let hex s =
  String.concat ""
    (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq s)))

let () =
  let open Solana_transaction in
  let keypair = Solana_crypto.keypair_of_seed (urandom 32) |> get "seed" in
  let payer = Solana_crypto.address keypair in
  let destination =
    Solana_crypto.keypair_of_seed (urandom 32) |> get "destination" |> Solana_crypto.address
  in
  let blockhash =
    Sys.getenv "SOLANA_BLOCKHASH" |> Solana_types.Hash.of_base58 |> get "blockhash"
  in
  let lamports = Solana_types.U64.of_string "5000" |> get "lamports" in
  let instructions =
    [
      Programs.set_compute_unit_limit 20_000 |> get "compute unit limit";
      Programs.transfer ~from:payer ~to_:destination ~lamports;
    ]
  in
  let message =
    Message.compile_legacy ~payer ~recent_blockhash:blockhash instructions |> get "compile"
  in
  Intent.derive message |> get "intent" |> Intent.validate_safe_sol_transfer |> get "intent";
  let signed =
    Transaction.create message |> Transaction.sign_with keypair |> get "sign"
    |> Transaction.finalize |> get "finalize"
  in
  Printf.printf "payer %s\nwire %s\n" (Solana_types.Address.to_base58 payer)
    (hex (Transaction.encode signed))
