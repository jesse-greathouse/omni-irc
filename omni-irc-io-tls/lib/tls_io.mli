(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
module Endpoint : sig
  type t = {
    host   : string;
    port   : int;
    sni    : string option;     (** default: Some host *)
    verify : bool;              (** default: true *)
    alpn   : string list;       (** default: [] *)
  }

  val make :
    host:string ->
    port:int ->
    ?sni:string ->
    ?verify:bool ->
    ?alpn:string list ->
    unit -> t
end

module IO : sig
  include Irc_sig.Io.S with type endpoint = Endpoint.t and type t = Tls_lwt.Unix.t
end
