type t = {
  client : Http_mirage_client.t;
  headers : (string * string) list;
  endpoint : string;
  max_response_bytes : int;
}

let header_exists name headers =
  List.exists
    (fun (candidate, _) ->
      String.lowercase_ascii candidate = String.lowercase_ascii name)
    headers

let create ?(headers = []) ?(max_response_bytes = 16 * 1024 * 1024) client
    endpoint =
  if max_response_bytes <= 0 then
    invalid_arg "max_response_bytes must be positive";
  let headers =
    if header_exists "content-type" headers then headers
    else ("content-type", "application/json") :: headers
  in
  { client; headers; endpoint; max_response_bytes }

let endpoint transport = transport.endpoint

type body_accumulator = {
  buffer : Buffer.t;
  mutable size : int;
  mutable exceeded : bool;
}

module Http_transport = struct
  type nonrec t = t
  type 'a io = 'a Lwt.t

  let return = Lwt.return
  let bind = Lwt.bind

  let post transport ~body =
    let accumulator =
      { buffer = Buffer.create (min transport.max_response_bytes 4096);
        size = 0; exceeded = false }
    in
    let add_chunk _response accumulator chunk =
      let chunk_length = String.length chunk in
      if
        (not accumulator.exceeded)
        && chunk_length <= transport.max_response_bytes - accumulator.size
      then (
        Buffer.add_string accumulator.buffer chunk;
        accumulator.size <- accumulator.size + chunk_length)
      else accumulator.exceeded <- true;
      Lwt.return accumulator
    in
    Lwt.bind
      (Http_mirage_client.request transport.client ~meth:`POST
         ~headers:transport.headers ~body transport.endpoint add_chunk accumulator)
      (function
        | Error error ->
          Lwt.return
            (Error
               (Format.asprintf "Mirage HTTP transport error: %a"
                  Mimic.pp_error error))
        | Ok (response, accumulator) ->
          if accumulator.exceeded then
            Lwt.return (Error "RPC response exceeds configured size limit")
          else
            let body = Buffer.contents accumulator.buffer in
            let status = Http_mirage_client.Status.to_code response.status in
            if status >= 200 && status < 300 then Lwt.return (Ok body)
            else
              Lwt.return
                (Error
                   (Printf.sprintf "HTTP %d from %s: %s" status
                      transport.endpoint body)))
end

module Provider = Solana_rpc.Provider.Of_http (Http_transport)
module Client = Solana_rpc.Provider.Make (Provider)
