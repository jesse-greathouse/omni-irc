(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module StringSet : Set.S with type elt = string

type t = {
  name   : string;
  topic  : string option;
  users  : StringSet.t;
  ops    : StringSet.t;
  voices : StringSet.t;
  modes  : string list;  (** channel modes like ["i"; "m"; "n"; ...] *)
}

val key_of_name : string -> string
val make : name:string -> t

val set_topic : t -> string option -> t

val set_modes  : t -> string list -> t
val add_mode   : t -> string -> t
val remove_mode: t -> string -> t

val add_user   : t -> string -> t
val add_op     : t -> string -> t
val add_voice  : t -> string -> t

val remove_user  : t -> string -> t
val remove_op    : t -> string -> t
val remove_voice : t -> string -> t

val remove_all : t -> string -> t

val has_user  : t -> string -> bool
val has_op    : t -> string -> bool
val has_voice : t -> string -> bool

val wire_of_name : string -> string
val to_wire      : t -> string
