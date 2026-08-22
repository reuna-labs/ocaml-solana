(* Run [mirage configure -t unix] here after installing the local packages.
   The linked application deliberately uses no Unix-facing Solana package. *)
open Mirage

let main = main "Unikernel.Main" ~packages:[ package "solana-transaction" ] job
let () = register "solana-offline-smoke" [ main ]
