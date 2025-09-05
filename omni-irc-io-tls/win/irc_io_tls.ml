(* Windows stub: provide Irc_io_tls.Tls_io used by main *)
open Lwt.Infix

module Tls_io = struct
  module Endpoint = struct
    type t = { host : string; port : int }
    let make ~host ~port () = { host; port }
  end
  module IO = struct
    type t = unit
    type endpoint = Endpoint.t
    let connect (_:endpoint) =
      Lwt.fail_with "TLS backend not available on this Windows build (install ocaml-tls stack or use --tls on Unix)."
    let recv  (_:t) (_:bytes) = Lwt.fail_with "TLS backend unavailable"
    let send  (_:t) ?off:_ ?len:_ (_:bytes) = Lwt.fail_with "TLS backend unavailable"
    let close (_:t) = Lwt.return_unit
  end
end
