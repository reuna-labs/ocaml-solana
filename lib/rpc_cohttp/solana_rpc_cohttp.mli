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

module Make (C : CLIENT) : sig
  type t
  val create : ?ctx:C.ctx -> ?headers:Http.Header.t -> ?max_response_bytes:int -> Uri.t -> t
  val uri : t -> Uri.t
  module Provider : Solana_rpc.Provider.S with type t = t and type 'a io = 'a Lwt.t
  module Client : sig
    val call : t -> 'a Solana_rpc.Method.t -> ('a, Solana_rpc.Error.t) result Lwt.t
  end
end
