(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
module IO : sig
  include Irc_sig.Io.S with type endpoint = string
  (** [endpoint] is the socket path. *)
end

type server

val serve_once : path:string -> perms:int -> (IO.t -> unit Lwt.t) -> server Lwt.t
val wait : server -> unit Lwt.t
val shutdown : server -> unit Lwt.t
