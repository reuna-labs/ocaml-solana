module Main = struct
  let start () =
    let get = function Ok value -> value | Error message -> failwith message in
    let payer = Solana_types.Address.of_bytes (String.make 32 '\001') |> get in
    let destination = Solana_types.Address.of_bytes (String.make 32 '\002') |> get in
    let recent_blockhash = Solana_types.Hash.of_bytes (String.make 32 '\003') |> get in
    let instruction =
      Solana_transaction.Programs.transfer ~from:payer ~to_:destination
        ~lamports:(Solana_types.U64.of_int 1 |> get)
    in
    ignore
      (Solana_transaction.Message.compile_legacy ~payer ~recent_blockhash [ instruction ]
       |> get |> Solana_transaction.Message.encode |> get)
end
