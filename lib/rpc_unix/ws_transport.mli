type t

val connect :
  ?extra_headers:Cohttp.Header.t ->
  ?random_string:(int -> string) ->
  ?ctx:Conduit_lwt_unix.ctx ->
  Conduit_lwt_unix.client ->
  Uri.t ->
  t Lwt.t

val read : t -> Websocket.Frame.t Lwt.t
val write : t -> Websocket.Frame.t -> unit Lwt.t
val close : t -> unit Lwt.t
