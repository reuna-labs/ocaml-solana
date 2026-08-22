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
