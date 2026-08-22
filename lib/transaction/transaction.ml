type t = { message : Message.t; signatures : Solana_types.Signature.t option array }
type signed = { message : Message.t; signatures : Solana_types.Signature.t array; wire : string }

let max_wire_size = 1232

let create message =
  let count = List.length (Message.required_signers message) in
  { message; signatures = Array.make count None }

let message (transaction : t) = transaction.message
let required_signers (transaction : t) = Message.required_signers transaction.message

let signing_bytes (transaction : t) = Message.encode transaction.message

let missing_signers (transaction : t) =
  List.mapi (fun index address -> index, address) (required_signers transaction)
  |> List.filter_map (fun (index, address) ->
         match transaction.signatures.(index) with None -> Some address | Some _ -> None)

let add_signature ~signer signature (transaction : t) =
  let rec find index = function
    | [] -> None
    | address :: rest ->
      if Solana_types.Address.equal address signer then Some index else find (index + 1) rest
  in
  match find 0 (required_signers transaction) with
  | None -> Error "transaction: signer is not required by this message"
  | Some index ->
    (match signing_bytes transaction with
    | Error _ as error -> error
    | Ok bytes ->
      if not (Solana_crypto.verify ~address:signer ~signature bytes) then
        Error "transaction: invalid Ed25519 signature"
      else
        let signatures = Array.copy transaction.signatures in
        signatures.(index) <- Some signature;
        Ok { transaction with signatures })

let sign_with keypair (transaction : t) =
  match signing_bytes transaction with
  | Error _ as error -> error
  | Ok bytes ->
    let signer = Solana_crypto.address keypair in
    let signature = Solana_crypto.sign keypair bytes in
    add_signature ~signer signature transaction

let assemble message signatures =
  match Shortvec.encode (Array.length signatures), Message.encode message with
  | Error message, _ | _, Error message -> Error message
  | Ok count, Ok message_bytes ->
    let buffer = Buffer.create (String.length message_bytes + (64 * Array.length signatures) + 3) in
    Buffer.add_string buffer count;
    Array.iter
      (fun signature -> Buffer.add_string buffer (Solana_types.Signature.to_bytes signature))
      signatures;
    Buffer.add_string buffer message_bytes;
    let wire = Buffer.contents buffer in
    if String.length wire > max_wire_size then
      Error
        (Printf.sprintf "transaction: wire size %d exceeds Solana limit %d"
           (String.length wire) max_wire_size)
    else Ok wire

let finalize (transaction : t) =
  if Array.exists Option.is_none transaction.signatures then
    Error "transaction: one or more required signatures are missing"
  else
    let signatures = Array.map Option.get transaction.signatures in
    match assemble transaction.message signatures with
    | Error _ as error -> error
    | Ok wire -> Ok { message = transaction.message; signatures; wire }

let encode (transaction : signed) = transaction.wire

let decode wire =
  if String.length wire > max_wire_size then
    Error
      (Printf.sprintf "transaction: wire size %d exceeds Solana limit %d"
         (String.length wire) max_wire_size)
  else
    match Shortvec.decode wire 0 with
    | Error _ as error -> error
    | Ok (signature_count, offset) ->
      let signatures_end = offset + (64 * signature_count) in
      if signatures_end > String.length wire then Error "transaction: truncated signatures"
      else
        let rec read index acc =
          if index = signature_count then Ok (Array.of_list (List.rev acc))
          else
            let bytes = String.sub wire (offset + (64 * index)) 64 in
            match Solana_types.Signature.of_bytes bytes with
            | Error _ as error -> error
            | Ok signature -> read (index + 1) (signature :: acc)
        in
        (match read 0 [] with
        | Error _ as error -> error
        | Ok signatures ->
          let message_bytes = String.sub wire signatures_end (String.length wire - signatures_end) in
          (match Message.decode message_bytes with
          | Error _ as error -> error
          | Ok message ->
            let signers = Message.required_signers message in
            if List.length signers <> signature_count then
              Error "transaction: signature count does not match message header"
            else if
              not
                (List.for_all2
                   (fun address signature -> Solana_crypto.verify ~address ~signature message_bytes)
                   signers (Array.to_list signatures))
            then Error "transaction: invalid Ed25519 signature"
            else Ok { message; signatures; wire }))

let of_signed (signed : signed) =
  { message = signed.message; signatures = Array.map (fun signature -> Some signature) signed.signatures }

let signed_message (signed : signed) = signed.message
let signatures (signed : signed) = Array.to_list signed.signatures

let id (signed : signed) =
  if Array.length signed.signatures = 0 then invalid_arg "transaction: signed transaction has no ID"
  else signed.signatures.(0)
