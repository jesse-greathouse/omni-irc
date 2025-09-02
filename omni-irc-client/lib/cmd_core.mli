(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module type CLIENT = sig
  type t
  val send_raw : t -> string -> unit Lwt.t
  val join     : t -> string -> unit Lwt.t
  val notify   : t -> string -> unit Lwt.t

  (** Request a channel list:
      - If cached (gate not expired), dump immediately.
      - If expired, clear cache, send LIST, and dump on 323. *)
  val list_request :
    t ->
    ?filter:string ->
    ?limit:int ->
    unit -> unit Lwt.t
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
