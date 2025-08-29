(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
open Lwt.Infix

module UIX = Irc_ui.Ui_intf
module Conn = Irc_conn.Connector
module type DIAL = Conn.DIAL

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
      let tcp_ep =
        Irc_io_tcp.Tcp_io.Endpoint.make ~host:ep.host ~port:ep.port ~tls:false
      in
      Base.connect tcp_ep

    let recv  = Base.recv
    let send  = Base.send
    let close = Base.close
  end
end

(*UI selection *)
let select_ui (name_opt : string option) : (module UIX.S) =
  match name_opt with
  | None | Some "" -> (module Irc_ui_notty.Ui : UIX.S)
  | Some name ->
      begin match String.lowercase_ascii name with
      | "notty" -> (module Irc_ui_notty.Ui : UIX.S)
      | _ -> prerr_endline ("Unknown UI '" ^ name ^ "'. Try: notty"); exit 2
      end

let () =
  (* CLI *)
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
    (* Choose NET backend at runtime, then instantiate the connector functor *)
    let (module Net : DIAL) =
      if !use_tls then (module Net_tls : DIAL) else (module Net_tcp : DIAL)
    in
    let module C = Conn.Make(Net) in

    let cfg : Conn.cfg = {
      host = !server;
      port = !port;
      username = None;
      password = None;
      realname = None;
      charset = None;
      keepalive = true;
    } in

    (* In-process queues between UI and network *)
    let to_client_m   : UIX.to_client Lwt_mvar.t   = Lwt_mvar.create_empty () in
    let from_client_m : UIX.from_client Lwt_mvar.t = Lwt_mvar.create_empty () in

    let ui_from_client () = Lwt_mvar.take from_client_m in
    let ui_to_client msg  = Lwt_mvar.put to_client_m msg in

    (* Start UI *)
    let ui_opt = if !ui_name = "" then None else Some !ui_name in
    let (module UIM : UIX.S) = select_ui ui_opt in
    let ui = UIM.create () in
    let ui_task =
      UIM.run ui ~from_client:ui_from_client ~to_client:ui_to_client
      >>= fun () -> UIM.close ui
    in

    (* Start NET *)
    let net_task =
      C.connect cfg >>= fun tconn ->
      Lwt_io.eprintf "[client] connected (%s).\n%!"
        (if !use_tls then "TLS" else "TCP") >>= fun () ->

      let send_line s =
        C.send tconn ~off:0 ~len:(String.length s) (Bytes.of_string s) >>= fun _ ->
        Lwt.return_unit
      in

      (* Optional initial handshake *)
      (if !nick <> "" then send_line (Printf.sprintf "NICK %s\r\n" !nick) else Lwt.return_unit) >>= fun () ->
      (if !realname <> "" then
        let rn = if !nick = "" then "guest" else !nick in
          send_line (Printf.sprintf "USER %s 0 * :%s\r\n" rn !realname)
      else Lwt.return_unit) >>= fun () ->

      let rec rx_loop () =
        let buf = Bytes.create 4096 in
        C.recv tconn buf >>= function
        | 0 -> Lwt_mvar.put from_client_m UIX.ClientClosed
        | n ->
          Lwt_mvar.put from_client_m (UIX.ClientRxChunk (Bytes.sub buf 0 n)) >>= rx_loop
      in

      let rec tx_loop () =
        Lwt_mvar.take to_client_m >>= function
        | UIX.UiSendRaw b ->
            C.send tconn ~off:0 ~len:(Bytes.length b) b >>= fun _ -> tx_loop ()
        | UIX.UiQuit ->
            Lwt.return_unit
      in

      Lwt.finalize
        (fun () -> Lwt.pick [ rx_loop (); tx_loop () ])
        (fun () ->
          Lwt_io.eprintf "[client] closing connection\n%!" >>= fun () ->
          Lwt.catch (fun () -> C.close tconn) (fun _ -> Lwt.return_unit))
    in

    Lwt.pick [ ui_task; net_task ]
  end
