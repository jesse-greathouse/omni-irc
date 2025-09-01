(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module type CLIENT = sig
  type t
  val send_raw : t -> string -> unit Lwt.t
  val join     : t -> string -> unit Lwt.t
  val notify   : t -> string -> unit Lwt.t
  val quit     : t -> unit Lwt.t

  val chanlist_upsert :
    t -> name:string -> num_users:int -> topic:string option -> unit Lwt.t

  val get_channels : t -> unit Lwt.t

  val evict_user : t -> string -> unit Lwt.t
end

module Make (P : Parser_intf.S) (C : CLIENT) : sig
  module Engine : module type of Engine.Make(P)
  val register_defaults : C.t Engine.t -> unit
end
