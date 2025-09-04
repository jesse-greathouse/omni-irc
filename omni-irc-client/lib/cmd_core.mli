(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module type CLIENT = sig
  type t
  val send_raw : t -> string -> unit Lwt.t
  val join     : t -> string -> unit Lwt.t
  val notify   : t -> string -> unit Lwt.t
  val list_request :
    t ->
    ?filter:string ->
    ?limit:int ->
    unit -> unit Lwt.t
  val channel_show : t -> string -> unit Lwt.t
  val whois_request : t -> string -> unit Lwt.t
  val sync_self_user : t -> unit Lwt.t 
end

module Make (C : CLIENT) : sig
  module Cmd : sig
    type ctx = C.t
    type t
    val create : unit -> t
    val dispatch : t -> ctx -> key:Cmd_key.t -> args:string list -> unit Lwt.t
    val dispatch_async : t -> ctx -> key:Cmd_key.t -> args:string list -> unit
  end
end
