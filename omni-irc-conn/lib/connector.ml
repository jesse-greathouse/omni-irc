(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
type cfg = Connector_intf.cfg
module type DIAL = Connector_intf.DIAL
module type S    = Connector_intf.S

module Make (D : DIAL)
  : S with type conn = D.IO.t
= struct
  type conn = D.IO.t

  let connect (cfg : cfg) =
    let ep = D.Endpoint.make ~host:cfg.host ~port:cfg.port in
    D.IO.connect ep

  let recv  = D.IO.recv
  let send  = D.IO.send
  let close = D.IO.close
end
