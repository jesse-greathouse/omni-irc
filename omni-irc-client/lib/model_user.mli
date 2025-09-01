(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
val key_of_nick : string -> string
(** Canonical, normalized key (lowercased, with any leading status char removed). *)

val normalize_nick : string -> string
(** Normalize an IRC nick to lowercase and strip a leading status marker if present
    (one of [@], [+], [%], [&], [~]). *)

(** WHOIS data captured on demand. Extend as needed. *)
type whois = {
  user        : string option;       (** ident / username *)
  host        : string option;       (** host *)
  realname    : string option;       (** realname from WHOIS *)
  server      : string option;       (** server providing the WHOIS info *)
  server_info : string option;       (** server's descriptive text *)
  account     : string option;       (** services account (if logged in) *)
  channels    : string list;         (** channels listed by WHOIS, wire names *)
  idle_secs   : int option;          (** idle time (seconds) *)
  signon_ts   : float option;        (** signon unix timestamp *)
}

(** Authoritative user object (single source of truth per client). *)
type t = {
  mutable nick      : string;          (** last-seen display casing for nick *)
  mutable real_name : string option;   (** real name if known (e.g. from 311/WHOIS) *)
  mutable ident     : string option;   (** username/ident (prefix user field) *)
  mutable host      : string option;   (** host (prefix host field) *)
  mutable account   : string option;   (** logged-in services account *)
  mutable away      : bool option;     (** away flag if known *)
  mutable whois     : whois option;    (** cached WHOIS payload *)
}

val make : ?real_name:string -> ?ident:string -> ?host:string ->
            ?account:string -> ?away:bool -> ?whois:whois -> string -> t

(** Optional: keep the old containers for non-channel use cases. *)
module Ord : sig
  type nonrec t = t
  val compare : t -> t -> int
end
module Set : Set.S with type elt = t
