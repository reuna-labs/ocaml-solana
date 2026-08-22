module type CLIENT = sig
  type ctx
  type body
  val body_of_string : string -> body
  val body_to_string : body -> string Lwt.t
  val post :
    ?ctx:ctx ->
    ?body:body ->
    ?chunked:bool ->
    ?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * body) Lwt.t
end

module Make (C : CLIENT) = struct
  type t = { ctx : C.ctx option; headers : Http.Header.t; uri : Uri.t; max_response_bytes : int }

  let create ?ctx ?(headers = Http.Header.init ()) ?(max_response_bytes = 16 * 1024 * 1024) uri =
    if max_response_bytes <= 0 then invalid_arg "max_response_bytes must be positive";
    let headers = Http.Header.add_unless_exists headers "content-type" "application/json" in
    { ctx; headers; uri; max_response_bytes }

  let uri transport = transport.uri

  module Http_transport = struct
    type nonrec t = t
    type 'a io = 'a Lwt.t
    let return = Lwt.return
    let bind = Lwt.bind

    let post transport ~body =
      Lwt.catch
        (fun () ->
          Lwt.bind
            (C.post ?ctx:transport.ctx ~headers:transport.headers
               ~body:(C.body_of_string body) transport.uri)
            (fun (response, response_body) ->
              Lwt.bind (C.body_to_string response_body) (fun response_body ->
                if String.length response_body > transport.max_response_bytes then
                  Lwt.return (Error "RPC response exceeds configured size limit")
                else
                  let status = Http.Response.status response |> Http.Status.to_int in
                  if status >= 200 && status < 300 then Lwt.return (Ok response_body)
                  else
                    Lwt.return
                      (Error
                         (Printf.sprintf "HTTP %d from %s: %s" status
                            (Uri.to_string transport.uri) response_body)))))
        (fun exception_ -> Lwt.return (Error (Printexc.to_string exception_)))
  end

  module Provider = Solana_rpc.Provider.Of_http (Http_transport)
  module Client = Solana_rpc.Provider.Make (Provider)
end
