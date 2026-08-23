(** Deterministic blockhash-refresh, signing, submission and confirmation state. *)

type config

val config :
  max_attempts:int ->
  max_confirmation_polls:int ->
  commitment:Solana_types.commitment ->
  (config, string) result

type failure =
  | Rpc of Error.t
  | Simulation_failed of Yojson.Safe.t
  | Transaction_failed of Yojson.Safe.t
  | Signature_mismatch of {
      expected : Solana_types.Signature.t;
      actual : Solana_types.Signature.t;
    }
  | Signed_with_wrong_blockhash of {
      expected : Solana_types.Hash.t;
      actual : Solana_types.Hash.t;
    }
  | Attempts_exhausted of int
  | Confirmation_timeout of int

type outcome =
  | Confirmed of Solana_types.Signature.t
  | Failed of failure

val pp_failure : Format.formatter -> failure -> unit

type action =
  | Fetch_latest_blockhash
  | Sign of Methods.latest_blockhash
  | Simulate of Solana_transaction.Transaction.signed
  | Submit of Solana_transaction.Transaction.signed
  | Check_signature of Solana_types.Signature.t
  | Check_block_height
  | Wait
  | Finished of outcome

type event =
  | Latest_blockhash of Methods.latest_blockhash Methods.contextual
  | Signed of Solana_transaction.Transaction.signed
  | Simulation of Methods.simulation Methods.contextual
  | Submitted of Solana_types.Signature.t
  | Signature_statuses of Methods.signature_status option list Methods.contextual
  | Block_height of Solana_types.U64.t
  | Waited
  | Rpc_error of Error.t
  | Deadline_reached

type t

val start : config -> t
val action : t -> action
val attempts_started : t -> int
val confirmation_polls : t -> int
val advance : t -> event -> (t, string) result

(** Recognises stale-blockhash errors in both messages and structured data. *)
val is_stale_blockhash_error : Error.t -> bool
