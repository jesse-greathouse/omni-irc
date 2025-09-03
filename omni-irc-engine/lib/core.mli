(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module type CLIENT = sig
  type t
  val send_raw : t -> string -> unit Lwt.t
  val join     : t -> string -> unit Lwt.t
  val notify   : t -> string -> unit Lwt.t
  val quit     : t -> unit Lwt.t
  val channel_set_topic : t -> ch:string -> topic:string -> unit Lwt.t

  val chanlist_upsert :
    t -> name:string -> num_users:int -> topic:string option -> unit Lwt.t
  val get_channels : t -> unit Lwt.t
  val evict_user : t -> string -> unit Lwt.t
  val list_completed : t -> unit Lwt.t

  (* NAMES flow *)
  val names_prepare   : t -> string -> unit Lwt.t
  val names_member    :
    t -> ch:string -> nick:string -> status:[ `Op | `Voice | `User ] -> unit Lwt.t
  val names_completed : t -> string -> unit Lwt.t

  (** membership changes from JOIN/PART *)
  val member_join : t -> ch:string -> nick:string -> unit Lwt.t
  val member_part : t -> ch:string -> nick:string -> reason:string option -> unit Lwt.t

  (** WHOIS updaters (called from Core on 311/312/319/338/671), and end signal (318). *)
  val whois_basic     : t -> nick:string -> user:string -> host:string -> realname:string option -> unit Lwt.t
  val whois_server    : t -> nick:string -> server:string -> server_info:string option -> unit Lwt.t
  val whois_channels  : t -> nick:string -> channels:string list -> unit Lwt.t
  val whois_actual    : t -> nick:string -> actual_host:string -> unit Lwt.t
  val whois_secure    : t -> nick:string -> unit Lwt.t
  val whois_complete  : t -> nick:string -> unit Lwt.t

  (** Mode tracking *)
  val channel_mode_change : t -> ch:string -> mode:string -> args:string list -> unit Lwt.t
  val user_mode_change    : t -> nick:string -> mode:string -> unit Lwt.t
end

module Make (P : Parser_intf.S) (C : CLIENT) : sig
  module E : module type of Engine.Make(P)
  val register_defaults : C.t E.t -> unit
end
