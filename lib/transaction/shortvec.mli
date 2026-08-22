(** Solana's strict compact u16 length encoding. *)

val encode : int -> (string, string) result
val decode : string -> int -> ((int * int), string) result
