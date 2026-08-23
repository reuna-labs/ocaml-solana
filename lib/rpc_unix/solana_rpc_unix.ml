module Unix_client :
  Solana_rpc_cohttp.CLIENT with type ctx = Cohttp_lwt_unix.Client.ctx =
struct
  type ctx = Cohttp_lwt_unix.Client.ctx
  type body = Cohttp_lwt_unix.Client.body

  let body_of_string = Cohttp_lwt.Body.of_string
  let body_to_stream = Cohttp_lwt.Body.to_stream

  let post ?ctx ?body ?chunked ?headers uri =
    Cohttp_lwt_unix.Client.post ?ctx ?body ?chunked ?headers uri
end

include Solana_rpc_cohttp.Make (Unix_client)

module Websocket = struct
  let uri_of_http uri =
    match Uri.scheme uri with
    | Some "http" -> Uri.with_scheme uri (Some "ws")
    | Some "https" -> Uri.with_scheme uri (Some "wss")
    | Some "ws" | Some "wss" -> uri
    | Some scheme -> invalid_arg ("unsupported RPC URI scheme: " ^ scheme)
    | None -> invalid_arg "RPC URI requires a scheme"

  let connect uri =
    let open Lwt.Infix in
    Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endpoint ->
    let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
    Conduit_lwt_unix.endp_to_client ~ctx endpoint >>= fun client ->
    Websocket_lwt_unix.connect ~ctx client uri

  let send connection (command : Solana_rpc.Subscriptions.command) =
    Websocket_lwt_unix.write connection
      (Websocket.Frame.create ~opcode:Websocket.Frame.Opcode.Text
         ~content:command.text ())

  let rec send_all connection = function
    | [] -> Lwt.return_unit
    | command :: rest ->
      Lwt.bind (send connection command) (fun () -> send_all connection rest)

  let receive_text connection =
    let buffer = Buffer.create 256 in
    let rec loop fragmented =
      Lwt.bind (Websocket_lwt_unix.read connection) (fun frame ->
        match frame.Websocket.Frame.opcode with
        | Ping ->
          Lwt.bind
            (Websocket_lwt_unix.write connection
               (Websocket.Frame.create ~opcode:Pong ~content:frame.content ()))
            (fun () -> loop fragmented)
        | Pong -> loop fragmented
        | Close ->
          Lwt.bind
            (Websocket_lwt_unix.write connection
               (Websocket.Frame.create ~opcode:Close ()))
            (fun () -> Lwt.fail End_of_file)
        | Text when fragmented ->
          Lwt.fail_with "WebSocket started a new text message before continuation"
        | Text ->
          Buffer.add_string buffer frame.content;
          if frame.final then Lwt.return (Buffer.contents buffer)
          else loop true
        | Continuation when not fragmented ->
          Lwt.fail_with "WebSocket continuation without an initial text frame"
        | Continuation ->
          Buffer.add_string buffer frame.content;
          if frame.final then Lwt.return (Buffer.contents buffer)
          else loop true
        | Binary | Ctrl _ | Nonctrl _ ->
          Lwt.fail_with "Solana WebSocket endpoint returned a non-text frame")
    in
    loop false

  let call_outputs on_output outputs =
    let rec loop = function
      | [] -> Lwt.return_unit
      | output :: rest ->
        Lwt.bind (on_output output) (fun () -> loop rest)
    in
    loop outputs

  type 'a stopped = Stop | Continue of 'a

  let with_stop stop promise =
    match stop with
    | None -> Lwt.map (fun value -> Continue value) promise
    | Some stop ->
      Lwt.pick
        [ Lwt.map (fun value -> Continue value) promise;
          Lwt.map (fun () -> Stop) stop ]

  let run ?(reconnect_delay_s = 0.5) ?stop
      ?(on_disconnect = fun _ -> Lwt.return_unit) uri initial_state ~on_output =
    if reconnect_delay_s < 0. then
      invalid_arg "reconnect_delay_s must be non-negative";
    let uri = uri_of_http uri in
    let rec reconnect state =
      let current_state = ref state in
      let connection = ref None in
      Lwt.catch
        (fun () ->
          Lwt.bind (with_stop stop (connect uri)) (function
            | Stop -> Lwt.return_unit
            | Continue established ->
              connection := Some established;
              let state, commands =
                Solana_rpc.Subscriptions.connected !current_state
              in
              current_state := state;
              let rec receive () =
                Lwt.bind (with_stop stop (receive_text established)) (function
                  | Stop -> Websocket_lwt_unix.close_transport established
                  | Continue text ->
                    (match
                       Solana_rpc.Subscriptions.receive !current_state text
                     with
                    | Error message -> Lwt.fail_with message
                    | Ok (state, outputs, commands) ->
                      current_state := state;
                      Lwt.bind (send_all established commands) (fun () ->
                        Lwt.bind (call_outputs on_output outputs) receive)))
              in
              Lwt.bind (send_all established commands) receive))
        (fun exception_ ->
          let close =
            match !connection with
            | None -> Lwt.return_unit
            | Some connection ->
              Lwt.catch
                (fun () -> Websocket_lwt_unix.close_transport connection)
                (fun _ -> Lwt.return_unit)
          in
          let state = Solana_rpc.Subscriptions.disconnected !current_state in
          Lwt.bind close (fun () ->
            Lwt.bind (on_disconnect (Printexc.to_string exception_)) (fun () ->
              Lwt.bind (with_stop stop (Lwt_unix.sleep reconnect_delay_s))
                (function
                  | Stop -> Lwt.return_unit
                  | Continue () -> reconnect state))))
    in
    reconnect initial_state
end
