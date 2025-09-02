(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

type t =
  | Join
  | Names
  | Raw
  | Nick
  | Privmsg
  | Get_list
  | Channel
  | WhoIs
  | Custom of string

let of_string s =
  match String.uppercase_ascii s with
  | "JOIN"  -> Join
  | "NAMES" -> Names
  | "RAW"   -> Raw
  | "NICK"  -> Nick
  | "PRIVMSG" -> Privmsg
  | "GET_LIST" -> Get_list
  | "CHANNEL"  -> Channel
  | "WHOIS"   -> WhoIs
  | "USER"    -> WhoIs
  | other   -> Custom other

let to_string = function
  | Join      -> "JOIN"
  | Names     -> "NAMES"
  | Raw       -> "RAW"
  | Nick      -> "NICK"
  | Privmsg   -> "PRIVMSG"
  | Get_list  -> "GET_LIST"
  | Channel   -> "CHANNEL"
  | WhoIs     -> "WHOIS"
  | Custom s  -> s
