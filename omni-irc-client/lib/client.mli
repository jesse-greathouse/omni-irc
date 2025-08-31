(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module P = Irc_engine.Parser
module UIX = Irc_ui.Ui_intf
module Conn = Irc_conn.Connector
module Engine : module type of Irc_engine.Engine.Make(P)

module type CONN = sig
  type conn
  val connect : Conn.cfg -> conn Lwt.t
  val recv    : conn -> bytes -> int Lwt.t
  val send    : conn -> ?off:int -> ?len:int -> bytes -> int Lwt.t
  val close   : conn -> unit Lwt.t
end

(** Replaceable command dispatcher contract *)
module type CMD = sig
  type ctx
  type t
  val create : unit -> t
  val dispatch : t -> ctx -> key:Cmd_key.t -> args:string list -> unit Lwt.t
  val dispatch_async : t -> ctx -> key:Cmd_key.t -> args:string list -> unit
end

type opts = { nick : string option; realname : string option }

type t

(** Existential pack to inject any CMD implementation bound to [ctx = t]. *)
type cmd_pack = Pack :
  (module CMD with type ctx = t and type t = 'a) * 'a -> cmd_pack

val default_cmd : unit -> cmd_pack

val create :
  (module CONN) ->
  Conn.cfg ->
  engine:t Engine.t ->
  ui:(module UIX.S) ->
  opts:opts ->
  ?cmd:cmd_pack ->
  unit -> t

val start  : t -> unit Lwt.t
val stop   : t -> unit Lwt.t

(* Public API used by handlers & UI/internal logic *)
val send_raw : t -> string -> unit Lwt.t
val join     : t -> string -> unit Lwt.t
val notify   : t -> string -> unit Lwt.t
val quit     : t -> unit Lwt.t

val cmd       : t -> key:Cmd_key.t -> args:string list -> unit Lwt.t
val cmd_async : t -> key:Cmd_key.t -> args:string list -> unit

(** Re-export for convenient call-sites: Client.Cmd_key.Join, etc. *)
module Cmd_key : module type of Cmd_key
