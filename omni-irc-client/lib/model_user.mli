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
  actual_host : string option;       (** RPL_WHOISACTUALLY / 338 *)
  secure      : bool option;         (** RPL_WHOISSECURE / 671 *)
}

(** Common IRC mode markers as constants (avoid naked strings). *)
module Mode : sig
  val operator_mode  : string 
  val voice_mode     : string
  val halfop_mode    : string
  val owner_mode     : string
  val admin_mode     : string
  val invisible_mode : string
end

(** Simple map for per-channel modes on a user. Keys are normalized channel names. *)
module StringMap : Map.S with type key = string

(** Authoritative user object (single source of truth per client). *)
type t = {
  mutable nick      : string;          (** last-seen display casing for nick *)
  mutable real_name : string option;   (** real name if known (e.g. from 311/WHOIS) *)
  mutable ident     : string option;   (** username/ident (prefix user field) *)
  mutable host      : string option;   (** host (prefix host field) *)
  mutable account   : string option;   (** logged-in services account *)
  mutable away      : bool option;     (** away flag if known *)
  mutable whois     : whois option;    (** cached WHOIS payload *)
  mutable modes     : string list;     (** global user modes, e.g. ["i"] *)
  mutable channel_modes : string list StringMap.t; (** per-channel modes, e.g. k->["o";"v"] *)
}

val make : ?real_name:string -> ?ident:string -> ?host:string ->
            ?account:string -> ?away:bool -> ?whois:whois ->
            ?modes:string list ->
            ?channel_modes:string list StringMap.t ->
            string -> t

(** Optional: keep the old containers for non-channel use cases. *)
module Ord : sig
  type nonrec t = t
  val compare : t -> t -> int
end
module Set : Set.S with type elt = t
