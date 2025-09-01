(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module type CLIENT = sig
  type t
  val send_raw : t -> string -> unit Lwt.t
  val join     : t -> string -> unit Lwt.t
  val notify   : t -> string -> unit Lwt.t
  val quit     : t -> unit Lwt.t
  val chanlist_upsert :
    t -> name:string -> num_users:int -> topic:string option -> unit Lwt.t
  val get_channels : t -> unit Lwt.t
  val evict_user : t -> string -> unit Lwt.t
end

module Make (P : Parser_intf.S) (C : CLIENT) = struct
  module Engine = Engine.Make(P)

  let h_ping (ev : P.event) (c : C.t) =
    match P.payload ev with
    | P.Ping { token } ->
        let tok = Option.value ~default:"" token in
        C.send_raw c (if tok = "" then "PONG\r\n" else Printf.sprintf "PONG :%s\r\n" tok)
    | _ -> Lwt.return_unit

  let h_invite_join (ev : P.event) (c : C.t) =
    match P.payload ev with
    | P.Invite { channel; _ } -> C.join c channel
    | _ -> Lwt.return_unit

  let h_invite_notify (ev : P.event) (c : C.t) =
    match P.payload ev with
    | P.Invite { channel; by } ->
        let who = Option.value ~default:"(unknown)" by in
        C.notify c (Printf.sprintf "You were invited by %s to join %s" who channel)
    | _ -> Lwt.return_unit

  (* 322 RPL_LIST -> upsert ChannelList *)
  let h_rpl_list (ev : P.event) (c : C.t) =
    match P.payload ev with
    | P.List_item { channel; num_users; topic } ->
        C.chanlist_upsert c ~name:channel ~num_users ~topic
    | _ -> Lwt.return_unit

  (* 376 RPL_ENDOFMOTD: no auto /LIST on connect *)
  let h_end_of_motd _ev _c = Lwt.return_unit
  
  let h_privmsg ev c =
    (* RFC1459-style channel prefixes: #, &, +, ! *)
    let is_channel_target (s : string) =
      match s with
      | "" -> false
      | _ ->
        match s.[0] with
        | '#' | '&' | '+' | '!' -> true
        | _ -> false
    in
    match P.payload ev with
    | P.Privmsg { from; target; text } when not (is_channel_target target) ->
        let who = Option.value ~default:"(unknown)" from in
        (* For a DM, target is your nick; no need to show it *)
        C.notify c (Printf.sprintf "<%s> %s" who text)
    | _ -> Lwt.return_unit

  let h_quit (ev : P.event) (c : C.t) =
    match P.payload ev with
    | P.Quit { who = Some who; _ } -> C.evict_user c who
    | P.Quit { who = None; _ } | _ -> Lwt.return_unit

  let h_kill (ev : P.event) (c : C.t) =
    match P.payload ev with
    | P.Kill { nick; _ } -> C.evict_user c nick
    | _ -> Lwt.return_unit

  let register_defaults eng =
    Engine.on eng "PING"          h_ping;
    Engine.on eng "INVITE"        h_invite_join;
    Engine.on eng "INVITE"        h_invite_notify;
    Engine.on eng "RPL_LIST"      h_rpl_list;
    Engine.on eng "RPL_ENDOFMOTD" h_end_of_motd;
    Engine.on eng "PRIVMSG"       h_privmsg;
    (* NEW: wire the eviction signals *)
    Engine.on eng "QUIT"          h_quit;
    Engine.on eng "KILL"          h_kill
end
