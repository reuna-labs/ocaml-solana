let immediate promise =
  match Lwt.state promise with
  | Lwt.Return value -> value
  | Lwt.Fail exception_ -> raise exception_
  | Lwt.Sleep -> Alcotest.fail "mock transport unexpectedly suspended"

module Mock_client = struct
  type ctx = unit
  type body = string list

  let response = ref (`OK, [])
  let body_of_string body = [ body ]
  let body_to_stream body = Lwt_stream.of_list body

  let post ?ctx:_ ?body:_ ?chunked:_ ?headers:_ _uri =
    let status, body = !response in
    Lwt.return (Http.Response.make ~status (), body)
end

module Transport = Solana_rpc_cohttp.Make (Mock_client)

let bounded_success () =
  Mock_client.response :=
    (`OK,
     [ {|{"jsonrpc":"2.0",|}; {|"id":0,"result":|};
       {|{"solana-core":"4.2.1"}}|} ]);
  let transport = Transport.create ~max_response_bytes:128 (Uri.of_string "https://rpc.invalid") in
  match immediate (Transport.Client.call transport (Solana_rpc.Methods.get_version ())) with
  | Ok version -> Alcotest.(check string) "version" "4.2.1" version.solana_core
  | Error error -> Alcotest.failf "%a" Solana_rpc.Error.pp error

let bounded_failure () =
  Mock_client.response := (`OK, [ "1234"; "5678"; String.make 1_000_000 'x' ]);
  let transport = Transport.create ~max_response_bytes:7 (Uri.of_string "https://rpc.invalid") in
  match immediate (Transport.Client.call transport (Solana_rpc.Methods.get_version ())) with
  | Error (Solana_rpc.Error.Transport message) ->
    Alcotest.(check string) "bounded error"
      "RPC response exceeds configured size limit" message
  | Error error -> Alcotest.failf "unexpected error: %a" Solana_rpc.Error.pp error
  | Ok _ -> Alcotest.fail "oversized streaming body was accepted"

let () =
  Alcotest.run "solana-rpc-cohttp"
    [ "streaming",
      [ Alcotest.test_case "bounded chunks" `Quick bounded_success;
        Alcotest.test_case "early size rejection" `Quick bounded_failure ] ]
