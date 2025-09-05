(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

let trim (s:string) =
  let n = String.length s in
  let i = ref 0 and j = ref (n - 1) in
  while !i < n && (match s.[!i] with ' ' | '\t' -> true | _ -> false) do incr i done;
  while !j >= !i && (match s.[!j] with ' ' | '\t' -> true | _ -> false) do decr j done;
  if !j < !i then "" else String.sub s !i (!j - !i + 1)

let split_words (s:string) =
  s |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")

let parse (line:string) : [ `Cmd of string * string list | `Raw of string ] =
  if String.length line > 0 && line.[0] = '/' then (
    let cmdline = trim (String.sub line 1 (String.length line - 1)) in
    if cmdline = "" then `Raw "" else
    match split_words cmdline with
    | [] -> `Raw ""
    | root :: args ->
      let r = String.lowercase_ascii root in
      match r with
      | "join" | "j"        -> `Cmd ("JOIN", args)
      | "names"             -> `Cmd ("NAMES", args)
      | "nick"              -> `Cmd ("NICK", args)
      | "msg" | "privmsg" ->
          (match args with
            | tgt :: rest -> `Cmd ("PRIVMSG", [tgt; String.concat " " rest])
            | [] -> `Cmd ("PRIVMSG", []))
      | "list"              -> `Cmd ("GET_LIST", args)
      | "raw"               -> `Cmd ("RAW", args)
      | "connect" | "c"     -> `Cmd ("CONNECT", args)
      | "user" | "whois"    -> `Cmd ("WHOIS", args)
      | "self" | "whoami"   -> `Cmd ("SELF", args)
      | other               -> `Cmd (String.uppercase_ascii other, args)
  ) else
    `Raw line
