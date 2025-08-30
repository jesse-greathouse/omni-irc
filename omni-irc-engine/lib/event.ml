(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

type payload =
  | Raw_line of string
  | Ping of { token : string option }
  | Invite of { channel : string; by : string option }
  | Other of string * string list

type t = { name : string; payload : payload }

let name e = e.name

let trim s =
  let n = String.length s in
  let i = ref 0 and j = ref (n - 1) in
  while !i < n
        && (s.[!i] = ' ' || s.[!i] = '\r' || s.[!i] = '\n' || s.[!i] = '\t') do
    incr i
  done;
  while !j >= !i
        && (s.[!j] = ' ' || s.[!j] = '\r' || s.[!j] = '\n' || s.[!j] = '\t') do
    decr j
  done;
  if !j < !i then "" else String.sub s !i (!j - !i + 1)

let of_irc_line line =
  let line = trim line in
  if line = "" then { name = "RAW"; payload = Raw_line line } else
  (* Split optional prefix *)
  let prefix, rest =
    if String.length line > 0 && line.[0] = ':' then
      match String.index_opt line ' ' with
      | None   -> (Some (String.sub line 1 (String.length line - 1)), "")
      | Some i -> (Some (String.sub line 1 (i - 1)),
                   String.sub line (i + 1) (String.length line - i - 1))
    else (None, line)
  in
  (* Split command / params / trailing *)
  let command, params, trailing =
    match String.index_opt rest ' ' with
    | None -> (String.uppercase_ascii rest, [], None)
    | Some sp ->
        let cmd   = String.uppercase_ascii (String.sub rest 0 sp) in
        let after = String.sub rest (sp + 1) (String.length rest - sp - 1) in
        (* find " :" (space then colon), or leading ':' *)
        let trailing_start =
          if String.length after > 0 && after.[0] = ':' then Some 0
          else
            let rec find i =
              if i + 1 >= String.length after then None
              else if after.[i] = ' ' && after.[i+1] = ':' then Some i
              else find (i + 1)
            in
            find 0
        in
        let before_trailing, trailing =
          match trailing_start with
          | None   -> (after, None)
          | Some 0 -> ("", Some (String.sub after 1 (String.length after - 1)))
          | Some i ->
              let bt = String.sub after 0 i in
              let tr = String.sub after (i + 2) (String.length after - i - 2) in
              (bt, Some tr)
        in
        let params =
          before_trailing
          |> String.split_on_char ' '
          |> List.filter (fun s -> s <> "")
        in
        (cmd, params, trailing)
  in
  match command, params, trailing with
  | "PING", _, t ->
      { name = "PING"; payload = Ping { token = t } }
  | "INVITE", _nick :: _, Some channel ->
      let by = prefix in
      { name = "INVITE"; payload = Invite { channel; by } }
  | cmd, ps, _ ->
      { name = cmd; payload = Other (cmd, ps) }
