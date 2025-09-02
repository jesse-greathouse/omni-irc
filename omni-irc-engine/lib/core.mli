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

  (** NEW: membership changes from JOIN/PART *)
  val member_join : t -> ch:string -> nick:string -> unit Lwt.t
  val member_part : t -> ch:string -> nick:string -> reason:string option -> unit Lwt.t
end

module Make (P : Parser_intf.S) (C : CLIENT) : sig
  module E : module type of Engine.Make(P)
  val register_defaults : C.t E.t -> unit
end
