module Make (IO : Irc_sig.Io.S) = struct
  type conn = IO.t

  type cfg = {
    host : string;
    port : int;
    username : string option;
    password : string option;
    realname : string option;
    charset : string option;
    tls : bool;
    keepalive : bool;
  }

  (* Map our cfg to the IO endpoint. This relies on the concrete IO’s endpoint type.
     For TCP we’ll define an endpoint record below; for other IOs you’ll adapt accordingly. *)
  let connect (_cfg : cfg) : conn Lwt.t =
    (* The functor can’t manufacture an endpoint unless the chosen IO’s endpoint
      is compatible with (host,port, tls, …). We therefore expect the concrete IO
      to expose an endpoint constructor in its mli (e.g. Tcp_io.Endpoint.make). *)
    Lwt.fail_with "omni-irc-conn: connect requires a concrete IO with an endpoint constructor"

  let recv (c : conn) (buf : bytes) = IO.recv c buf
  let send (c : conn) ?off ?len b = IO.send c ?off ?len b
  let close (c : conn) = IO.close c
end
