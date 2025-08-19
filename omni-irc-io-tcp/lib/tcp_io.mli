(** TCP IO (optionally future-TLS) for Omni IRC. *)

module Endpoint : sig
		type t = {
		host : string;
		port : int;
		tls  : bool;
		(* room for SNI, ALPN, timeouts, etc. *)
	}
	val make : host:string -> port:int -> tls:bool -> t
end

module IO : sig
	include Irc_sig.Io.S with type endpoint = Endpoint.t and type t = Lwt_unix.file_descr
end
