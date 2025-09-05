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
    | Motd_end of { message : string option } (** 376 RPL_ENDOFMOTD *)
    | List_item of { channel : string; num_users : int; topic : string option } (** 322 *)
    | List_end  of { message : string option } (** 323 RPL_LISTEND *)
    | Privmsg  of { from : string option; target : string; text : string }
    | Quit     of { who : string option; message : string option }
    | Kill     of { nick : string; reason : string option }
    | Join     of { channel : string; nick : string option; reason : string option }
    | Part     of { channel : string; nick : string option; reason : string option }
    | Nick_change of { old_nick : string option; new_nick : string }
    | Other    of string * string list * string option

  val payload : event -> payload

  module Acc : sig
    include module type of Acc
    val flush_remainder : t -> string option
  end
end
