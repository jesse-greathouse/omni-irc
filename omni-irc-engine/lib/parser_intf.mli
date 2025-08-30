(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

(** Minimal surface the engine depends on. *)
module type ENGINE = sig
  type event
  val name    : event -> string
  val of_line : string -> event
  module Acc : sig
    type t
    val create     : unit -> t
    val push_chunk : t -> string -> event list
  end
end

(** Full parser contract for Core/bots: extends ENGINE with a concrete payload. *)
module type S = sig
  include ENGINE

  type payload =
    | Raw_line of string
    | Ping     of { token : string option }
    | Invite   of { channel : string; by : string option }
    | Other    of string * string list  (* command, params *)

  val payload : event -> payload

  module Acc : sig
    include module type of Acc
    val flush_remainder : t -> string option
  end
end
