module type S = sig
  type t
  type 'a io
  val return : 'a -> 'a io
  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val request : t -> method_:string -> params:Yojson.Safe.t list ->
    (Yojson.Safe.t, Error.t) result io
end

module Make (P : S) : sig
  val call : P.t -> 'a Method.t -> ('a, Error.t) result P.io
end

module type HTTP = sig
  type t
  type 'a io
  val return : 'a -> 'a io
  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val post : t -> body:string -> (string, string) result io
end

module Of_http (H : HTTP) : S with type t = H.t and type 'a io = 'a H.io
