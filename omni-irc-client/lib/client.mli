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
val channel_set_topic : t -> ch:string -> topic:string -> unit Lwt.t

(** Singular “self” user support *)
val set_self_by_nick : t -> string -> unit Lwt.t
val sync_self_user   : t -> unit Lwt.t

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
(** Legacy, time-gated “send LIST then wait briefly” function (still available). *)

(** New flow for /list: gate + async emit on 323. *)
val list_request :
  t ->
  ?filter:string ->
  ?limit:int ->
  unit -> unit Lwt.t

val whois_request : t -> string -> unit Lwt.t

(** Called by Core on 323 to dump the table to the UI (using last requested args). *)
val list_completed : t -> unit Lwt.t

(** For direct dumps in other contexts (kept for convenience). *)
val get_and_emit_channels :
  t ->
  ?filter:string ->
  ?limit:int ->
  unit -> unit Lwt.t

(** Re-export for convenient call-sites: Client.Cmd_key.Join, etc. *)
module Cmd_key : module type of Cmd_key

(** Users: single source of truth on the client *)
val user_find   : t -> string -> User.t option
val user_ensure : t -> string -> User.t
val users_size  : t -> int
val prune_orphan_members : t -> unit
val evict_user_sync : t -> string -> unit
val evict_user_by_key : t -> string -> unit Lwt.t
val evict_user : t -> string -> unit Lwt.t

(** Channels → UI synchronization blobs (CLIENT JSON). *)
val emit_channels_snapshot : t -> unit Lwt.t
val emit_channels_upsert   : t -> names:string list -> unit Lwt.t
val emit_channels_remove   : t -> names:string list -> unit Lwt.t

(** Emit a single-channel CLIENT blob (type = "channel") for [name].
    If the client is not in the channel, a user-facing error is notified. *)
val emit_channel_info : t -> name:string -> unit Lwt.t

(** NAMES 353/366 helpers used by core handlers *)
val names_prepare   : t -> string -> unit Lwt.t
val names_member    :
  t -> ch:string -> nick:string -> status:[ `Op | `Voice | `User ] -> unit Lwt.t

val names_completed : t -> string -> unit Lwt.t

(** update per-channel membership on JOIN/PART *)
val member_join : t -> ch:string -> nick:string -> unit Lwt.t
val member_part : t -> ch:string -> nick:string -> reason:string option -> unit Lwt.t

val channel_mode_change : t -> ch:string -> mode:string -> args:string list -> unit Lwt.t
val user_mode_change    : t -> nick:string -> mode:string -> unit Lwt.t

(** WHOIS mutation helpers (called by Core) *)
val whois_basic     : t -> nick:string -> user:string -> host:string -> realname:string option -> unit Lwt.t
val whois_server    : t -> nick:string -> server:string -> server_info:string option -> unit Lwt.t
val whois_channels  : t -> nick:string -> channels:string list -> unit Lwt.t
val whois_actual    : t -> nick:string -> actual_host:string -> unit Lwt.t
val whois_secure    : t -> nick:string -> unit Lwt.t
val whois_complete  : t -> nick:string -> unit Lwt.t
