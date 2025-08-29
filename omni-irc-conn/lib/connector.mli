(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
type cfg = Connector_intf.cfg
module type DIAL = Connector_intf.DIAL
module type S    = Connector_intf.S

module Make (_ : DIAL) : S
