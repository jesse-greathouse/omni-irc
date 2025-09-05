(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

(** Parse a UI input line. If it begins with '/', return a command tuple
    (uppercase key, args). Otherwise return the raw line (no CRLF). *)
val parse :
  string ->
  [ `Cmd of string * string list | `Raw of string ]
