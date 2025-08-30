(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

type payload =
  | Raw_line of string
  | Ping of { token : string option }
  | Invite of { channel : string; by : string option }
  | Other of string * string list  (* command, params *)

type t = { name : string; payload : payload }

val name : t -> string

(** Very small/forgiving parser:
    - Recognizes "PING :token"
    - Recognizes ":{prefix} INVITE <nick> :#channel" (prefix optional)
    - Otherwise returns Other/Raw_line.
    This is intentionally tiny; you can swap in a full parser later. *)
val of_irc_line : string -> t
