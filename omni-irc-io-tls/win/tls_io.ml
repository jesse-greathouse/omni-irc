(* Windows stub: present the same API shape but fail if used *)

module Endpoint = struct
  type t = { host : string; port : int }
  let make ~host ~port () = { host; port }
end

module IO = struct
  type t = unit
  type endpoint = Endpoint.t

  let fail () =
    Lwt.fail_with "TLS is not available on Windows in this build (omni-irc-io-tls stub). Use --no-tls."

  let connect (_:endpoint) = fail ()
  let recv    _ _ = fail ()
  let send    _ ?off:_ ?len:_ _ = fail ()
  let close   _ = Lwt.return_unit
end
