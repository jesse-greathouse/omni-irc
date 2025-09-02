(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

open Lwt.Infix

module type CLIENT = sig
  type t
  val send_raw : t -> string -> unit Lwt.t
  val join     : t -> string -> unit Lwt.t
  val notify   : t -> string -> unit Lwt.t
  val quit     : t -> unit Lwt.t
  val channel_set_topic : t -> ch:string -> topic:string -> unit Lwt.t
  val chanlist_upsert :
    t -> name:string -> num_users:int -> topic:string option -> unit Lwt.t
  val get_channels : t -> unit Lwt.t
  val evict_user : t -> string -> unit Lwt.t
  val list_completed : t -> unit Lwt.t

  val names_prepare   : t -> string -> unit Lwt.t
  val names_member    :
    t -> ch:string -> nick:string -> status:[ `Op | `Voice | `User ] -> unit Lwt.t
  val names_completed : t -> string -> unit Lwt.t

  val member_join : t -> ch:string -> nick:string -> unit Lwt.t
  val member_part : t -> ch:string -> nick:string -> reason:string option -> unit Lwt.t
end

module Make (P : Parser_intf.S) (C : CLIENT) = struct
  module E = Engine.Make(P)

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

  (* 323 RPL_LISTEND -> emit ChannelList snapshot to UI *)
  let h_rpl_list_end (_ev : P.event) (c : C.t) =
    C.list_completed c

  (* 353 RPL_NAMREPLY: accumulate members into the channel *)
  let h_rpl_names (ev : P.event) (c : C.t) =
    let classify tok =
      if tok = "" then (`User, tok) else
      match tok.[0] with
      | '@' | '&' | '~' | '%' -> (`Op,    String.sub tok 1 (String.length tok - 1))
      | '+'                   -> (`Voice, String.sub tok 1 (String.length tok - 1))
      | _                     -> (`User, tok)
    in
    match P.payload ev with
    | P.Other ("353", params, trailing) ->
        let ch_opt =
          match params with
          | _me :: _sym :: ch :: _ -> Some ch
          | _me :: ch :: _         -> Some ch   (* fallback / non-standard ordering *)
          | _                      -> None
        in
        begin match (ch_opt, trailing) with
        | (Some ch, Some names_line) ->
            let tokens =
              names_line
              |> String.split_on_char ' '
              |> List.filter (fun s -> s <> "")
            in
            C.names_prepare c ch >>= fun () ->
            Lwt_list.iter_s
              (fun tok ->
                let (status, nick) = classify tok in
                C.names_member c ~ch ~nick ~status)
              tokens
        | _ -> Lwt.return_unit
        end
    | _ -> Lwt.return_unit

  (* 366 RPL_ENDOFNAMES: flush the finished channel to the UI/client *)
  let h_rpl_names_end (ev : P.event) (c : C.t) =
    match P.payload ev with
    | P.Other ("366", params, _msg) ->
        let ch_opt =
          match params with
          | _me :: ch :: _ -> Some ch
          | _              -> None
        in
        (match ch_opt with Some ch -> C.names_completed c ch | None -> Lwt.return_unit)
    | _ -> Lwt.return_unit

  (* 332 RPL_TOPIC: update the channel topic *)
  let h_rpl_topic (ev : P.event) (c : C.t) =
    match P.payload ev with
    | P.Other ("332", params, topic_opt) ->
        let ch_opt =
          match params with
          | _me :: ch :: _ -> Some ch
          | _              -> None
        in
        (match (ch_opt, topic_opt) with
         | (Some ch, Some topic) -> C.channel_set_topic c ~ch ~topic
         | _ -> Lwt.return_unit)
    | _ -> Lwt.return_unit

  (* 376 RPL_ENDOFMOTD: no auto /LIST on connect *)
  let h_end_of_motd _ev _c = Lwt.return_unit

  let h_privmsg ev c =
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

  let h_join (ev : P.event) (c : C.t) =
    match P.payload ev with
    | P.Join { channel; nick = Some nick; _ } ->
        C.member_join c ~ch:channel ~nick
    | P.Join _ -> Lwt.return_unit
    | _ -> Lwt.return_unit

  let h_part (ev : P.event) (c : C.t) =
    match P.payload ev with
    | P.Part { channel; nick = Some nick; reason } ->
        C.member_part c ~ch:channel ~nick ~reason
    | P.Part _ -> Lwt.return_unit
    | _ -> Lwt.return_unit

  let register_defaults eng =
    E.on eng "332"           h_rpl_topic;
    E.on eng "PING"          h_ping;
    E.on eng "INVITE"        h_invite_join;
    E.on eng "INVITE"        h_invite_notify;
    E.on eng "RPL_LIST"      h_rpl_list;
    E.on eng "RPL_LISTEND"   h_rpl_list_end;
    E.on eng "353"           h_rpl_names;
    E.on eng "366"           h_rpl_names_end;
    E.on eng "RPL_ENDOFMOTD" h_end_of_motd;
    E.on eng "PRIVMSG"       h_privmsg;
    E.on eng "QUIT"          h_quit;
    E.on eng "KILL"          h_kill;
    E.on eng "JOIN"          h_join;
    E.on eng "PART"          h_part
end
