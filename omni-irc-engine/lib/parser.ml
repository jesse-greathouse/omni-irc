(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

type payload =
  | Raw_line of string
  | Ping     of { token : string option }
  | Invite   of { channel : string; by : string option }
  | Motd_end of { message : string option }     (* 376 *)
  | List_item of { channel : string; num_users : int; topic : string option }  (* 322 *)
  | List_end  of { message : string option }    (* 323 *)
  | Privmsg  of { from : string option; target : string; text : string }
  | Quit     of { who : string option; message : string option }
  | Kill     of { nick : string; reason : string option }
  | Join     of { channel : string; nick : string option; reason : string option }
  | Part     of { channel : string; nick : string option; reason : string option }
  | Other    of string * string list * string option

type event = { name : string; payload : payload }

let name e = e.name
let payload e = e.payload

(* Extract the nick from an IRC prefix like "nick!user@host" *)
let nick_of_prefix (p : string option) : string option =
  match p with
  | None -> None
  | Some s ->
      match String.index_opt s '!' with
      | Some i -> Some (String.sub s 0 i)
      | None   -> Some s

(* local helpers, intentionally private *)
let trim s =
  let n = String.length s in
  let i = ref 0 and j = ref (n - 1) in
  while !i < n && (match s.[!i] with ' ' | '\r' | '\n' | '\t' -> true | _ -> false) do incr i done;
  while !j >= !i && (match s.[!j] with ' ' | '\r' | '\n' | '\t' -> true | _ -> false) do decr j done;
  if !j < !i then "" else String.sub s !i (!j - !i + 1)

let of_line line =
  let line = trim line in
  (* strip IRCv3 tags if present *)
  let line =
    if String.length line > 0 && line.[0] = '@' then
      match String.index_opt line ' ' with
      | Some i -> String.sub line (i + 1) (String.length line - i - 1)
      | None   -> ""
    else line
  in
  if line = "" then { name = "RAW"; payload = Raw_line line } else
  (* Optional prefix *)
  let prefix, rest =
    if String.length line > 0 && line.[0] = ':' then
      match String.index_opt line ' ' with
      | None   -> (Some (String.sub line 1 (String.length line - 1)), "")
      | Some i -> (Some (String.sub line 1 (i - 1)),
                    String.sub line (i + 1) (String.length line - i - 1))
    else (None, line)
  in
  (* Command / params / trailing *)
  let command, params, trailing =
    match String.index_opt rest ' ' with
    | None -> (String.uppercase_ascii rest, [], None)
    | Some sp ->
        let cmd   = String.uppercase_ascii (String.sub rest 0 sp) in
        let after = String.sub rest (sp + 1) (String.length rest - sp - 1) in
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
          before_trailing |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
        in
        (cmd, params, trailing)
  in
  match command, params, trailing with
  | "PING", _, t ->
      { name = "PING"; payload = Ping { token = t } }
  | "JOIN", (ch :: _), reason ->
      { name = "JOIN";
        payload = Join { channel = ch; nick = nick_of_prefix prefix; reason } }
  | "JOIN", [], (Some ch) ->
      { name = "JOIN";
        payload = Join { channel = ch; nick = nick_of_prefix prefix; reason = None } }
  | "INVITE", _nick :: ch :: _, _ ->
      { name = "INVITE"; payload = Invite { channel = ch; by = prefix } }
  | "INVITE", _nick :: _, Some ch ->
      { name = "INVITE"; payload = Invite { channel = ch; by = prefix } }
  | "376", _me :: _, msg ->
      { name = "RPL_ENDOFMOTD"; payload = Motd_end { message = msg } }
  | "322", _me :: ch :: users :: _, topic ->
      let num_users = try int_of_string users with _ -> 0 in
      { name = "RPL_LIST"; payload = List_item { channel = ch; num_users; topic } }
  | "323", _me :: _, msg ->
      { name = "RPL_LISTEND"; payload = List_end { message = msg } }
  | "PRIVMSG", target :: _, Some text ->
      { name = "PRIVMSG"; payload = Privmsg { from = prefix; target; text } }
  | "PART", (ch :: _), reason ->
      { name = "PART";
        payload = Part { channel = ch; nick = nick_of_prefix prefix; reason } }
  | "PART", [], (Some ch) ->
      { name = "PART";
        payload = Part { channel = ch; nick = nick_of_prefix prefix; reason = None } }
  | "QUIT", _, msg ->
      { name = "QUIT"; payload = Quit { who = prefix; message = msg } }
  | "KILL", nick :: _, reason ->
      { name = "KILL"; payload = Kill { nick; reason } }
  | cmd, ps, trailing ->
      { name = cmd; payload = Other (cmd, ps, trailing) }

module Acc = struct
  type t = { buf : Buffer.t }
  let create () = { buf = Buffer.create 4096 }

  let push_chunk t chunk =
    Buffer.add_string t.buf chunk;
    let s = Buffer.contents t.buf in
    let parts = String.split_on_char '\n' s in
    let rev = List.rev parts in
    let tail, complete =
      match rev with
      | [] -> ("", [])
      | last :: rev_init -> (last, List.rev rev_init)
    in
    Buffer.clear t.buf;
    Buffer.add_string t.buf tail;
    List.map (fun l ->
      let l =
        if String.length l > 0 && l.[String.length l - 1] = '\r'
        then String.sub l 0 (String.length l - 1) else l
      in
      of_line l) complete

  let flush_remainder t =
    let s = Buffer.contents t.buf in
    if s = "" then None else Some s
end
