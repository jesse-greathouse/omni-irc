(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module User = Model_user

type t = {
  name   : string;         (** normalized name, WITHOUT leading '#'/'&' *)
  topic  : string option;
  users  : User.Set.t;     (** present users *)
  ops    : User.Set.t;     (** channel operators *)
  voices : User.Set.t;     (** +v *)
}

(** Key used for maps: strip leading '#'/'&' and lowercase. *)
val key_of_name : string -> string

(** Create an empty channel record; [name] must be normalized (no prefix). *)
val make : name:string -> t

val set_topic : t -> string option -> t

val add_user   : t -> User.t -> t
val add_op     : t -> User.t -> t
val add_voice  : t -> User.t -> t

val remove_user  : t -> User.t -> t
val remove_op    : t -> User.t -> t
val remove_voice : t -> User.t -> t

val has_user  : t -> User.t -> bool
val has_op    : t -> User.t -> bool
val has_voice : t -> User.t -> bool

(** Convert to/from “wire” names (with leading '#' when missing). *)
val wire_of_name : string -> string
val to_wire      : t -> string
