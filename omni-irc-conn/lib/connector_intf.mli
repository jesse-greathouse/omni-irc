(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
type cfg = {
  host      : string;
  port      : int;
  username  : string option;
  password  : string option;
  realname  : string option;
  charset   : string option;
  keepalive : bool;
}

module type DIAL = sig
  module Endpoint : sig
    type t
    val make : host:string -> port:int -> t
  end
  module IO : sig
    include Irc_sig.Io.S with type endpoint = Endpoint.t
  end
end

module type S = sig
  type conn
  val connect : cfg -> conn Lwt.t
  val recv    : conn -> bytes -> int Lwt.t
  val send    : conn -> ?off:int -> ?len:int -> bytes -> int Lwt.t
  val close   : conn -> unit Lwt.t
end
