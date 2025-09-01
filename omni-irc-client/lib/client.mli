(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

(* Re-exports for convenience *)
module P = Irc_engine.Parser
module UIX = Irc_ui.Ui_intf
module Conn = Irc_conn.Connector
module Engine : module type of Irc_engine.Engine.Make(P)

(** Re-export models for convenience. *)
module User : module type of Model_user
module Channel : module type of Model_channel
module Channel_list : module type of Model_channel_list

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

(** Channels model accessors *)
val channel_find   : t -> string -> Channel.t option
val channel_ensure : t -> string -> Channel.t

(** ChannelList (from RPL_LIST 322) accessors *)
val channel_list_find   : t -> string -> Channel_list.entry option
val channel_list_upsert :
  t -> name:string -> num_users:int -> topic:string option -> unit

val get_channels : t -> Channel_list.t Lwt.t
(** Return the current channel list; if the 3-minute gate has expired,
    send LIST and wait ~2s for updates before returning. *)

(** For the UI `/list` command: refresh if the gate allows, wait the settle window
    when we did refresh, then emit a readable dump (one line per channel).  Two
    optional positional filters are supported:
      [/list <string> <number>]
    - <string>: case-insensitive substring on channel name; use "*" or omit for no filter
    - <number>: limit the number of rows; omit for no limit *)
val get_and_emit_channels :
  t ->
  ?filter:string ->
  ?limit:int ->
  unit ->
  unit Lwt.t

(** Re-export for convenient call-sites: Client.Cmd_key.Join, etc. *)
module Cmd_key : module type of Cmd_key

(** Users: single source of truth on the client *)
val user_find   : t -> string -> User.t option
val user_ensure : t -> string -> User.t
val users_size  : t -> int
val prune_orphan_members : t -> unit
val evict_user_sync : t -> string -> unit

(** Evict a user by normalized key (lowercased nick, no mode prefix). *)
val evict_user_by_key : t -> string -> unit Lwt.t

(** Evict a user by *nick* (may include mode prefix). *)
val evict_user : t -> string -> unit Lwt.t
