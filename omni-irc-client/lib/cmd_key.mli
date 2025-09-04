(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

(** Canonical, typed command keys. Avoid stringly-typed usage. *)
type t =
  | Join
  | Names
  | Raw
  | Nick
  | Privmsg
  | Get_list
  | Channel
  | WhoIs
  | Self
  | Custom of string  (** catch-all / extension point *)

val of_string : string -> t
(** Map incoming text to a key. Case-insensitive for known keys. *)

val to_string : t -> string
(** Round-trip to an uppercase IRC-ish symbol for known keys; Custom as-is. *)
