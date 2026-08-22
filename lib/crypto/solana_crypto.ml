module E = Mirage_crypto_ec.Ed25519

type keypair = {
  seed : string;
  private_key : E.priv;
  address : Solana_types.Address.t;
}

let keypair_of_seed seed =
  if String.length seed <> 32 then
    Error (Printf.sprintf "ed25519 seed: expected 32 bytes, got %d" (String.length seed))
  else
    match E.priv_of_octets seed with
    | Error error -> Error (Format.asprintf "%a" Mirage_crypto_ec.pp_error error)
    | Ok private_key ->
      let public = E.pub_to_octets (E.pub_of_priv private_key) in
      (match Solana_types.Address.of_bytes public with
      | Error _ as error -> error
      | Ok address -> Ok { seed; private_key; address })

let seed keypair = keypair.seed
let address keypair = keypair.address

let sign keypair message =
  match Solana_types.Signature.of_bytes (E.sign ~key:keypair.private_key message) with
  | Ok signature -> signature
  | Error message -> invalid_arg message

let verify ~address ~signature message =
  match E.pub_of_octets (Solana_types.Address.to_bytes address) with
  | Error _ -> false
  | Ok key -> E.verify ~key (Solana_types.Signature.to_bytes signature) ~msg:message
