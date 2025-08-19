open Lwt.Infix

module Endpoint = struct
  type t = {
    host : string;
    port : int;
    tls  : bool;
  }
  let make ~host ~port ~tls = { host; port; tls }
end

module IO : Irc_sig.Io.S with type endpoint = Endpoint.t and type t = Lwt_unix.file_descr = struct
  type t = Lwt_unix.file_descr
  type endpoint = Endpoint.t

  let connect (ep : endpoint) =
    let open Unix in
    let hints = [ AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM ] in
    Lwt_unix.getaddrinfo ep.host (string_of_int ep.port) hints >>= function
    | [] -> Lwt.fail_with ("omni-irc-io-tcp: no addrinfo for " ^ ep.host)
    | ai :: _ ->
      let fd = Lwt_unix.socket ai.ai_family ai.ai_socktype ai.ai_protocol in
      Lwt_unix.connect fd ai.ai_addr >>= fun () ->
      if ep.tls then
        (* Placeholder: integrate tls-lwt here later. *)
        Lwt.fail_with "omni-irc-io-tcp: TLS not implemented yet"
      else
        Lwt.return fd

  let recv fd buf =
    Lwt_unix.read fd buf 0 (Bytes.length buf)

  let send fd ?(off=0) ?len buf =
    let len = match len with Some l -> l | None -> Bytes.length buf - off in
    Lwt_unix.write fd buf off len

  let close fd = Lwt_unix.close fd
end
