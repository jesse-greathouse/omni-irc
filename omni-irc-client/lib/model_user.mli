(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

(** Minimal user model (can be expanded later). *)
type t = { nick : string }

val make : string -> t

(** Normalize irc nick for comparisons (lowercase; strips leading mode marks like @,+). *)
val normalize_nick : string -> string

(** Ordering and containers (case-insensitive by normalized nick). *)
module Ord : sig
  type nonrec t = t
  val compare : t -> t -> int
end

module Set : Set.S with type elt = t
