open Lwt.Infix

module Framing = Websocket.Make (Cohttp_lwt_unix.Private.IO)

type t = {
  read_frame : unit -> Websocket.Frame.t Lwt.t;
  write_frame : Websocket.Frame.t -> unit Lwt.t;
  input : Lwt_io.input_channel;
  output : Lwt_io.output_channel;
}

exception Http_error of string

let fail_unless condition exception_ =
  if condition then Lwt.return_unit else Lwt.fail exception_

let handshake request input output nonce =
  let open Cohttp in
  Framing.Request.write ~flush:true (fun _writer -> Lwt.return_unit) request
    output
  >>= fun () ->
  Framing.Response.read input >>= function
  | `Eof -> Lwt.fail End_of_file
  | `Invalid reason -> Lwt.fail (Failure reason)
  | `Ok response ->
    let status = Response.status response in
    let headers = Response.headers response in
    fail_unless
      (not (Code.is_error (Code.code_of_status status)))
      (Http_error (Code.string_of_status status))
    >>= fun () ->
    fail_unless
      (Response.version response = `HTTP_1_1)
      (Websocket.Protocol_error "wrong HTTP version")
    >>= fun () ->
    fail_unless
      (status = `Switching_protocols)
      (Websocket.Protocol_error "wrong HTTP status")
    >>= fun () ->
    fail_unless
      (match Header.get headers "upgrade" with
      | Some value -> String.lowercase_ascii value = "websocket"
      | None -> false)
      (Websocket.Protocol_error "wrong Upgrade header")
    >>= fun () ->
    fail_unless
      (Websocket.upgrade_present headers)
      (Websocket.Protocol_error "Connection header does not contain Upgrade")
    >>= fun () ->
    let expected =
      Websocket.b64_encoded_sha1sum (nonce ^ Websocket.websocket_uuid)
    in
    fail_unless
      (Cohttp.Header.get headers "sec-websocket-accept" = Some expected)
      (Websocket.Protocol_error "wrong Sec-WebSocket-Accept header")

let secure_random length = Mirage_crypto_rng_unix.getrandom length

let connect ?(extra_headers = Cohttp.Header.init ())
    ?(random_string = secure_random)
    ?(ctx = Lazy.force Conduit_lwt_unix.default_ctx) client uri =
  let nonce = Base64.encode_exn (random_string 16) in
  let headers =
    Cohttp.Header.add_list extra_headers
      [ ("Upgrade", "websocket");
        ("Connection", "Upgrade");
        ("Sec-WebSocket-Key", nonce);
        ("Sec-WebSocket-Version", "13") ]
  in
  let request = Cohttp.Request.make ~headers uri in
  Conduit_lwt_unix.connect ~ctx client >>= fun (_flow, raw_input, output) ->
  let input = Cohttp_lwt_unix.Private.Input_channel.create raw_input in
  Lwt.catch
    (fun () -> handshake request input output nonce)
    (fun exception_ ->
      Lwt_io.close raw_input >>= fun () ->
      Lwt.fail exception_)
  >|= fun () ->
  let read_frame =
    Framing.make_read_frame ~mode:(Framing.Client random_string) input output
  in
  let read_frame () =
    Lwt.catch read_frame (fun exception_ ->
      Lwt.async (fun () -> Lwt_io.close raw_input);
      Lwt.fail exception_)
  in
  let buffer = Buffer.create 128 in
  let write_frame frame =
    Buffer.clear buffer;
    Lwt.wrap2
      (Framing.write_frame_to_buf ~mode:(Framing.Client random_string))
      buffer frame
    >>= fun () ->
    Lwt.catch
      (fun () ->
        Lwt_io.write output (Buffer.contents buffer) >>= fun () ->
        Lwt_io.flush output)
      (fun exception_ ->
        Lwt.async (fun () -> Lwt_io.close output);
        Lwt.fail exception_)
  in
  { read_frame; write_frame; input = raw_input; output }

let read t = t.read_frame ()
let write t = t.write_frame

let close t =
  Lwt.catch
    (fun () -> Lwt_io.close t.output)
    (fun _ -> Lwt_io.close t.input)
