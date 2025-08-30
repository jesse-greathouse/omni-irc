(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module UIX = Irc_ui.Ui_intf
module Conn = Irc_conn.Connector
module Engine = Irc_engine.Engine

module type CONN = sig
  type conn
  val connect : Conn.cfg -> conn Lwt.t
  val recv    : conn -> bytes -> int Lwt.t
  val send    : conn -> ?off:int -> ?len:int -> bytes -> int Lwt.t
  val close   : conn -> unit Lwt.t
end

type opts = {
  nick     : string option;
  realname : string option;
}

type t

val create :
  (module CONN) ->
  Conn.cfg ->
  engine:t Engine.t -> (* NOTE: the engine context is [t] *)
  ui:(module UIX.S) ->
  opts:opts ->
  t

val start  : t -> unit Lwt.t
val stop   : t -> unit Lwt.t

(* Public API used by handlers *)
val send_raw : t -> string -> unit Lwt.t
val join     : t -> string -> unit Lwt.t
val notify   : t -> string -> unit Lwt.t
val quit     : t -> unit Lwt.t
