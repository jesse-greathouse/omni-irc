(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module type CLIENT = sig
  type t
  val send_raw : t -> string -> unit Lwt.t
  val join     : t -> string -> unit Lwt.t
  val notify   : t -> string -> unit Lwt.t
  val list_request :
    t -> ?filter:string -> ?limit:int -> unit -> unit Lwt.t
  val channel_show :
    t -> string -> unit Lwt.t
  val whois_request : t -> string -> unit Lwt.t
end

module Make (C : CLIENT) = struct
  module Cmd = struct
    type ctx = C.t
    type t = unit
    let create () = ()

    let ensure_channel s =
      if s = "" then s else
      match s.[0] with '#' | '&' -> s | _ -> "#" ^ s

    let do_join (c:ctx) = function
      | ch :: _ -> C.join c (ensure_channel ch)
      | []      -> C.notify c "JOIN requires a channel argument"

    let do_names (c:ctx) = function
      | []      -> C.send_raw c "NAMES\r\n"
      | ch :: _ -> C.send_raw c (Printf.sprintf "NAMES %s\r\n" (ensure_channel ch))

    let do_whois (c:ctx) = function
      | nick :: _ when String.trim nick <> "" ->
          C.whois_request c (String.trim nick)
      | _ ->
          C.notify c "WHOIS requires a <nick>"

    let do_raw (c:ctx) (args:string list) =
      let line = String.concat " " args in
      let line =
        if String.length line >= 2
          && line.[String.length line - 2] = '\r'
          && line.[String.length line - 1] = '\n'
        then line else line ^ "\r\n"
      in
      C.send_raw c line

    let do_nick (c:ctx) = function
      | newnick :: _ -> C.send_raw c (Printf.sprintf "NICK %s\r\n" newnick)
      | [] -> C.notify c "NICK requires a <newnick>"

    let do_privmsg (c:ctx) = function
      | target :: rest ->
          let msg = String.concat " " rest in
          if msg = "" then C.notify c "MSG requires a <target> and <message>"
          else C.send_raw c (Printf.sprintf "PRIVMSG %s :%s\r\n" target msg)
      | [] ->
          C.notify c "MSG requires a <target> and <message>"

    (* /list [pattern] [rows] *)
    let do_get_list (c:ctx) (args:string list) =
      let filter_opt =
        match args with
        | [] -> None
        | a :: _ ->
            let a = String.trim a in
            if a = "" || a = "*" then None else Some a
      in
      let limit_opt =
        match args with
        | [] | [_] -> None
        | _ :: b :: _ ->
            (try let n = int_of_string (String.trim b) in if n > 0 then Some n else None
              with _ -> None)
      in
      C.list_request c ?filter:filter_opt ?limit:limit_opt ()

    (* /channel <#ch> — required arg *)
    let do_channel (c:ctx) = function
      | ch :: _ ->
          let ch = ensure_channel ch in
          C.channel_show c ch
      | [] ->
          C.notify c "CHANNEL requires a channel argument"

    let dispatch _t (c:ctx) ~key ~args =
      let f =
        match key with
        | Cmd_key.Join      -> do_join
        | Cmd_key.Names     -> do_names
        | Cmd_key.Raw       -> do_raw
        | Cmd_key.Nick      -> do_nick
        | Cmd_key.Privmsg   -> do_privmsg
        | Cmd_key.Get_list  -> do_get_list
        | Cmd_key.Channel   -> do_channel
        | Cmd_key.WhoIs     -> do_whois
        | Cmd_key.Custom _  -> (fun _ _ -> Lwt.return_unit)
      in
      Lwt.catch (fun () -> f c args) (fun _ -> Lwt.return_unit)

    let dispatch_async t c ~key ~args =
      Lwt.async (fun () -> dispatch t c ~key ~args)
  end
end
