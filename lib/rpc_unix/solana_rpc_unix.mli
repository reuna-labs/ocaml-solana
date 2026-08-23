type t

val create :
  ?ctx:Cohttp_lwt_unix.Client.ctx ->
  ?headers:Http.Header.t ->
  ?max_response_bytes:int ->
  Uri.t ->
  t

val uri : t -> Uri.t

module Provider :
  Solana_rpc.Provider.S with type t = t and type 'a io = 'a Lwt.t

module Client : sig
  val call :
    t ->
    'a Solana_rpc.Method.t ->
    ('a, Solana_rpc.Error.t) result Lwt.t
end

module Websocket : sig
  val uri_of_http : Uri.t -> Uri.t

  (** Maintains desired signature and account subscriptions across transport
      failures. The optional stop promise cancels a blocked receive or retry
      delay. *)
  val run :
    ?reconnect_delay_s:float ->
    ?stop:unit Lwt.t ->
    ?on_disconnect:(string -> unit Lwt.t) ->
    Uri.t ->
    Solana_rpc.Subscriptions.t ->
    on_output:(Solana_rpc.Subscriptions.output -> unit Lwt.t) ->
    unit Lwt.t
end
