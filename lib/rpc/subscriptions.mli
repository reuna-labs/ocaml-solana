(** Solana WebSocket subscription protocol with deterministic reconnect state. *)

type local_id = int
type account_encoding = Base64 | Json_parsed

type subscription =
  | Signature of {
      signature : Solana_types.Signature.t;
      commitment : Solana_types.commitment;
      enable_received_notification : bool;
    }
  | Account of {
      address : Solana_types.Address.t;
      commitment : Solana_types.commitment;
      encoding : account_encoding;
    }

type command = { text : string }

type output =
  | Subscribed of { local_id : local_id; server_id : int }
  | Unsubscribed of local_id
  | Notification of { local_id : local_id; value : Yojson.Safe.t }
  | Server_error of { request_id : int; error : Yojson.Safe.t }

type t

val create : unit -> t
val is_connected : t -> bool
val desired_count : t -> int
val active_count : t -> int
val add : t -> subscription -> t * local_id * command list
val remove : t -> local_id -> t * command list

(** Emits a fresh request for every desired subscription. Server subscription
    identifiers are deliberately discarded across a reconnect. *)
val connected : t -> t * command list
val disconnected : t -> t

val receive : t -> string -> (t * output list * command list, string) result
