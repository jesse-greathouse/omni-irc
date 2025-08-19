(** Omni IO tiny signature (duplex byte stream) *)

module type S = sig
  type t
  val recv  : t -> bytes -> int Lwt.t
  val send  : t -> ?off:int -> ?len:int -> bytes -> int Lwt.t
  val close : t -> unit Lwt.t
end
