(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module Make (P : Parser_intf.ENGINE) : sig
  type event = P.event
  type 'ctx handler = event -> 'ctx -> unit Lwt.t
  type 'ctx t

  (* Re-expose the parser’s accumulator with type equality, limited to what we use *)
  module Acc : sig
    type t = P.Acc.t
    val create : unit -> t
    val push_chunk : t -> string -> event list
  end

  val create     : unit -> 'ctx t
  val on         : 'ctx t -> string -> 'ctx handler -> unit
  val emit       : 'ctx t -> event -> 'ctx -> unit Lwt.t
  val emit_async : 'ctx t -> event -> 'ctx -> unit

  val ingest_line  : 'ctx t -> string -> 'ctx -> unit Lwt.t
  val ingest_chunk : 'ctx t -> Acc.t -> string -> 'ctx -> unit Lwt.t
end
