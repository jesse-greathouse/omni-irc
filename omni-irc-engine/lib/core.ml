(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module type CLIENT = sig
  type t
  val send_raw : t -> string -> unit Lwt.t
  val join     : t -> string -> unit Lwt.t
  val notify   : t -> string -> unit Lwt.t
  val quit     : t -> unit Lwt.t
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

  let register_defaults eng =
    Engine.on eng "PING"   h_ping;
    Engine.on eng "INVITE" h_invite_join;
    Engine.on eng "INVITE" h_invite_notify
end
