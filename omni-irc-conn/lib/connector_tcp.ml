module Make (Tcp : sig
  module Endpoint : sig
    type t

    val make : host:string -> port:int -> tls:bool -> t
  end

  module IO : sig
    include Irc_sig.Io.S with type endpoint = Endpoint.t
  end
end) =
struct
  (* Use the wrapped name of the base connector functor *)
  module C = Irc_conn.Connector.Make (Tcp.IO)

  type cfg = C.cfg
  type conn = C.conn

  let connect (cfg : cfg) : conn Lwt.t =
    let ep = Tcp.Endpoint.make ~host:cfg.host ~port:cfg.port ~tls:cfg.tls in
    Tcp.IO.connect ep

  let recv = C.recv
  let send = C.send
  let close = C.close
end
