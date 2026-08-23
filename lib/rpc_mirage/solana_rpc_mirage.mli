(** MirageOS HTTP/1.1 or HTTP/2 TLS transport using the platform ALPN client. *)

type t

val create :
  ?headers:(string * string) list ->
  ?max_response_bytes:int ->
  Http_mirage_client.t ->
  string ->
  t

val endpoint : t -> string

module Provider :
  Solana_rpc.Provider.S with type t = t and type 'a io = 'a Lwt.t

module Client : sig
  val call :
    t ->
    'a Solana_rpc.Method.t ->
    ('a, Solana_rpc.Error.t) result Lwt.t
end
