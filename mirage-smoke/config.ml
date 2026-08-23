(* Run [mirage configure -t unix] here after installing the local packages. *)
open Mirage

let main =
  main "Unikernel.Make" ~packages:[ package "solana-rpc-mirage" ]
    (alpn_client @-> job)

let stack = generic_stackv4v6 default_network
let happy_eyeballs = generic_happy_eyeballs stack
let dns = generic_dns_client stack happy_eyeballs
let mimic = mimic_happy_eyeballs stack happy_eyeballs dns
let client = paf_client (tcpv4v6_of_stackv4v6 stack) mimic

let () = register "solana-rpc-smoke" [ main $ client ]
