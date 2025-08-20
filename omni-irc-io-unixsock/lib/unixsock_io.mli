(** Single-consumer AF_UNIX IO implementation & tiny relay helpers. This exposes an [IO.S] instance
    that treats a connected AF_UNIX socket as a duplex byte stream (no IRC framing, no parsing). *)

module IO : sig
  include Irc_sig.Io.S with type endpoint = string
  (** [endpoint] is the socket path. *)
end

type server

val serve_once : path:string -> perms:int -> (IO.t -> unit Lwt.t) -> server Lwt.t
val wait : server -> unit Lwt.t
val shutdown : server -> unit Lwt.t
