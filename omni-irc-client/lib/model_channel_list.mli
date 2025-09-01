(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

(** Channel list entry as returned by RPL_LIST (322). *)
type entry = {
  name      : string;           (** normalized, no leading '#'/'&' *)
  num_users : int;
  topic     : string option;
}

(** Key used for maps: strip leading '#'/'&' and lowercase. *)
val key_of_name : string -> string

module Map : Map.S with type key = string
type t = entry Map.t

val empty : t
val find  : string -> t -> entry option
val upsert : name:string -> num_users:int -> topic:string option -> t -> t

(** “Wire” representation, adding '#' if missing. *)
val wire_of_name : string -> string
