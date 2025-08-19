(** Omni IO tiny signature (duplex byte stream with constructor) *)

module type S = sig
  type t

  (** Abstract “where to connect” for this IO implementation. *)
  type endpoint

  (** Establish a connection and return a live duplex handle. *)
  val connect : endpoint -> t Lwt.t

  (** Read up to [Bytes.length buf] bytes; returns 0 on EOF. *)
  val recv  : t -> bytes -> int Lwt.t

  (** Write [len] bytes starting at [off] (defaults to the full buffer). *)
  val send  : t -> ?off:int -> ?len:int -> bytes -> int Lwt.t

  (** Close the handle. Idempotent. *)
  val close : t -> unit Lwt.t
end
