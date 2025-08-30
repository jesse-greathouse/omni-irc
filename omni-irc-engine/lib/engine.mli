(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
type 'ctx handler = Event.t -> 'ctx -> unit Lwt.t
type 'ctx t = (string, 'ctx handler list) Hashtbl.t

val create : unit -> 'ctx t
val on     : 'ctx t -> string -> 'ctx handler -> unit
val emit   : 'ctx t -> Event.t -> 'ctx -> unit Lwt.t
val emit_async : 'ctx t -> Event.t -> 'ctx -> unit
