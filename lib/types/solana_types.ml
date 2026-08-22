module U64 = struct
  type t = Z.t

  let max = Z.pred (Z.shift_left Z.one 64)
  let zero = Z.zero

  let of_z value =
    if Z.sign value < 0 || Z.gt value max then Error "u64: value outside [0, 2^64-1]"
    else Ok value

  let of_int value = of_z (Z.of_int value)

  let of_string value =
    match Z.of_string value with
    | value -> of_z value
    | exception Invalid_argument _ -> Error "u64: invalid decimal integer"

  let to_z value = value
  let to_string = Z.to_string
  let compare = Z.compare
  let pp formatter value = Format.pp_print_string formatter (to_string value)
end

module Fixed = struct
  let of_bytes ~name ~length value =
    if String.length value = length then Ok value
    else Error (Printf.sprintf "%s: expected %d bytes, got %d" name length (String.length value))

  let of_base58 ~name ~length value =
    match Web3_codec_base58.decode value with
    | Error message -> Error (name ^ ": " ^ message)
    | Ok bytes -> of_bytes ~name ~length bytes
end

module Address = struct
  type t = string

  let of_bytes = Fixed.of_bytes ~name:"address" ~length:32
  let to_bytes value = value
  let of_base58 = Fixed.of_base58 ~name:"address" ~length:32
  let to_base58 = Web3_codec_base58.encode
  let compare = String.compare
  let equal = String.equal
  let pp formatter value = Format.pp_print_string formatter (to_base58 value)
end

module Hash = struct
  type t = string

  let of_bytes = Fixed.of_bytes ~name:"hash" ~length:32
  let to_bytes value = value
  let of_base58 = Fixed.of_base58 ~name:"hash" ~length:32
  let to_base58 = Web3_codec_base58.encode
  let equal = String.equal
  let pp formatter value = Format.pp_print_string formatter (to_base58 value)
end

module Signature = struct
  type t = string

  let of_bytes = Fixed.of_bytes ~name:"signature" ~length:64
  let to_bytes value = value
  let of_base58 = Fixed.of_base58 ~name:"signature" ~length:64
  let to_base58 = Web3_codec_base58.encode
  let equal = String.equal
  let pp formatter value = Format.pp_print_string formatter (to_base58 value)
end

type commitment = Processed | Confirmed | Finalized

let commitment_to_string = function
  | Processed -> "processed"
  | Confirmed -> "confirmed"
  | Finalized -> "finalized"

let commitment_of_string = function
  | "processed" -> Ok Processed
  | "confirmed" -> Ok Confirmed
  | "finalized" -> Ok Finalized
  | value -> Error ("commitment: unsupported value " ^ value)

module Network = struct
  type t = { name : string; genesis_hash : Hash.t }

  let make ~name ~genesis_hash = { name; genesis_hash }

  let known name encoded =
    match Hash.of_base58 encoded with
    | Ok genesis_hash -> { name; genesis_hash }
    | Error message -> invalid_arg message

  let devnet = known "devnet" "GH7ome3EiwEr7tu9JuTh2dpYWBJK3z69Xm1ZE3MEE6JC"
  let testnet = known "testnet" "4uhcVJyU9pJkvQyS88uRDiswHXSCkY3zQawwpjk2NsNY"
  let mainnet_beta = known "mainnet-beta" "5eykt4UsFv8P8NJdTREpY1vzqKqZKvdpKuc147dw2N9d"
end

type account_meta = { address : Address.t; signer : bool; writable : bool }

let account_meta ?(signer = false) ?(writable = false) address = { address; signer; writable }

type instruction = {
  program : Address.t;
  accounts : account_meta list;
  data : string;
}

let instruction ~program ~accounts ~data = { program; accounts; data }
