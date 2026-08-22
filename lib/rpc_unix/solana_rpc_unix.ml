module Unix_client :
  Solana_rpc_cohttp.CLIENT with type ctx = Cohttp_lwt_unix.Client.ctx =
struct
  type ctx = Cohttp_lwt_unix.Client.ctx
  type body = Cohttp_lwt_unix.Client.body

  let body_of_string = Cohttp_lwt.Body.of_string
  let body_to_string = Cohttp_lwt.Body.to_string

  let post ?ctx ?body ?chunked ?headers uri =
    Cohttp_lwt_unix.Client.post ?ctx ?body ?chunked ?headers uri
end

include Solana_rpc_cohttp.Make (Unix_client)
