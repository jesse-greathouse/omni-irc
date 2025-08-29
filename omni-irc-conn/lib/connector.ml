(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
module type DIAL = sig
  module Endpoint : sig
    type t
    val make : host:string -> port:int -> t
  end
  module IO : sig
    include Irc_sig.Io.S with type endpoint = Endpoint.t
  end
end

module Make (D : DIAL) = struct
  type conn = D.IO.t

  type cfg = {
    host      : string;
    port      : int;
    username  : string option;
    password  : string option;
    realname  : string option;
    charset   : string option;
    keepalive : bool;
  }

  let connect (cfg : cfg) : conn Lwt.t =
    let ep = D.Endpoint.make ~host:cfg.host ~port:cfg.port in
    D.IO.connect ep

  let recv  = D.IO.recv
  let send  = D.IO.send
  let close = D.IO.close
end
