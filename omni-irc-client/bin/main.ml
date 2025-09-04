(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
open Lwt.Infix

module P          = Irc_engine.Parser
module UIX        = Irc_ui.Ui_intf
module Conn       = Irc_conn.Connector
module type DIAL  = Conn.DIAL
module Engine     = Irc_engine.Engine.Make(P)
module Core       = Irc_engine.Core.Make(P)
module Client     = Irc_client.Client

(* TLS backend as a DIAL *)
module Net_tls : DIAL = struct
  module Endpoint = struct
    type t = Irc_io_tls.Tls_io.Endpoint.t
    let make ~host ~port = Irc_io_tls.Tls_io.Endpoint.make ~host ~port ()
  end
  module IO = Irc_io_tls.Tls_io.IO
end

(* Plain TCP backend as a DIAL *)
module Net_tcp : DIAL = struct
  module Endpoint = struct
    type t = { host : string; port : int }
    let make ~host ~port = { host; port }
  end
  module IO = struct
    module Base = Irc_io_tcp.Tcp_io.IO
    type t = Base.t
    type endpoint = Endpoint.t
    let connect (ep : endpoint) =
      let open Endpoint in
      let tcp_ep = Irc_io_tcp.Tcp_io.Endpoint.make ~host:ep.host ~port:ep.port in
      Base.connect tcp_ep
    let recv  = Base.recv
    let send  = Base.send
    let close = Base.close
  end
end

let select_ui (name_opt : string option) : (module UIX.S) =
  match name_opt with
  | None | Some "" -> (module Irc_ui_notty.Ui : UIX.S)
  | Some name ->
      begin match String.lowercase_ascii name with
      | "notty" -> (module Irc_ui_notty.Ui : UIX.S)
      | _ -> prerr_endline ("Unknown UI '" ^ name ^ "'. Try: notty"); exit 2
      end

let () =
  let server    = ref "" in
  let port      = ref 0 in
  let nick      = ref "" in
  let realname  = ref "" in
  let ui_name   = ref "" in
  let use_tls   = ref false in

  let specl =
    [
      ("--server",   Arg.Set_string server,   "IRC server hostname (required)");
      ("--port",     Arg.Set_int    port,     "IRC server port (required)");
      ("--nick",     Arg.Set_string nick,     "IRC nickname (optional init)");
      ("--realname", Arg.Set_string realname, "IRC realname (optional init)");
      ("--ui",       Arg.Set_string ui_name,  "UI adapter (default: notty)");
      ("--tls",      Arg.Set use_tls,         "Enable TLS (ocaml-tls backend)");
    ]
  in
  let usage = "omni-irc-client --server HOST --port PORT [--ui notty] [--tls]" in
  Arg.parse specl (fun _ -> ()) usage;

  let missing what () = prerr_endline ("missing required arg: " ^ what); exit 2 in
  if !server = "" then missing "--server" ();
  if !port   <= 0 then missing "--port" ();

  Lwt_main.run begin
    let (module Net : DIAL) =
      if !use_tls then (module Net_tls : DIAL) else (module Net_tcp : DIAL)
    in
    let module C = Conn.Make(Net) in

    let cfg : Conn.cfg = {
      host = !server;
      port = !port;
      username = None;
      password = None;
      realname = (if !realname = "" then None else Some !realname);
      charset = None;
      keepalive = true;
    } in

    let ui_opt = if !ui_name = "" then None else Some !ui_name in
    let ui_mod = select_ui ui_opt in

    let (eng : Client.t Engine.t) = Engine.create () in

    let module Core_for_client = Core(struct
        type t = Client.t
        let send_raw = Client.send_raw
        let join     = Client.join
        let notify   = Client.notify
        let quit     = Client.quit
        let channel_set_topic c ~ch ~topic = Client.channel_set_topic c ~ch ~topic
        let chanlist_upsert c ~name ~num_users ~topic =
          Client.channel_list_upsert c ~name ~num_users ~topic; Lwt.return_unit
        let get_channels c =
          Client.get_channels c >>= fun _ -> Lwt.return_unit
        let evict_user = Client.evict_user
        let list_completed = Client.list_completed
        let names_prepare   = Client.names_prepare
        let names_member    = Client.names_member
        let names_completed = Client.names_completed
        let member_join c ~ch ~nick = Client.member_join c ~ch ~nick
        let member_part c ~ch ~nick ~reason = Client.member_part c ~ch ~nick ~reason
        let whois_basic    = Client.whois_basic
        let whois_server   = Client.whois_server
        let whois_channels = Client.whois_channels
        let whois_actual   = Client.whois_actual
        let whois_secure   = Client.whois_secure
        let whois_complete = Client.whois_complete
        let channel_mode_change = Client.channel_mode_change
        let user_mode_change    = Client.user_mode_change
      end)
    in
    Core_for_client.register_defaults eng;

    let client =
      Client.create
        (module C)
        cfg
        ~engine:eng
        ~ui:ui_mod
        ~opts:{ Client.nick     = (if !nick = "" then None else Some !nick);
                Client.realname = (if !realname = "" then None else Some !realname) }
        ()
    in
    Lwt_io.eprintf "[client] using %s\n%!"
      (if !use_tls then "TLS transport" else "TCP transport") >>= fun () ->
    Client.start client
  end
