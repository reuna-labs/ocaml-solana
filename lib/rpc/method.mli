type 'a t

val make : name:string -> ?params:Yojson.Safe.t list ->
  (Yojson.Safe.t -> ('a, string) result) -> 'a t
val name : 'a t -> string
val params : 'a t -> Yojson.Safe.t list
val decode : 'a t -> Yojson.Safe.t -> ('a, string) result
