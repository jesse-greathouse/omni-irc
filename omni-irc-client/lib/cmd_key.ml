(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

type t =
  | Join
  | Names
  | Raw
  | Custom of string

let of_string s =
  match String.uppercase_ascii s with
  | "JOIN"  -> Join
  | "NAMES" -> Names
  | "RAW"   -> Raw
  | other   -> Custom other

let to_string = function
  | Join      -> "JOIN"
  | Names     -> "NAMES"
  | Raw       -> "RAW"
  | Custom s  -> s
