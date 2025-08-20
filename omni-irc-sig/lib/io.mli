(** Omni IO tiny signature (duplex byte stream with constructor) *)

module type S = sig
  type t

  type endpoint
  (** Abstract “where to connect” for this IO implementation. *)

  val connect : endpoint -> t Lwt.t
  (** Establish a connection and return a live duplex handle. *)

  val recv : t -> bytes -> int Lwt.t
  (** Read up to [Bytes.length buf] bytes; returns 0 on EOF. *)

  val send : t -> ?off:int -> ?len:int -> bytes -> int Lwt.t
  (** Write [len] bytes starting at [off] (defaults to the full buffer). *)

  val close : t -> unit Lwt.t
  (** Close the handle. Idempotent. *)
end
