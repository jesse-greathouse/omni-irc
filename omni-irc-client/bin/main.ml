open Lwt.Infix

(* TCP dialer via connector *)
module Tcp = struct
  module Endpoint = Irc_io_tcp.Tcp_io.Endpoint
  module IO = Irc_io_tcp.Tcp_io.IO
end

module Conn = Irc_conn_tcp.Connector_tcp.Make (Tcp)

module UIX = Irc_ui.Ui_intf

(* Add a runtime selector that returns a first-class module implementing Ui_intf.S *)
let select_ui (name : string) : (module UIX.S) =
  match String.lowercase_ascii name with
  | "notty" -> (module Irc_ui_notty.Ui : UIX.S)
  (* Future adapters:
     | "stdio" -> (module Irc_ui_stdio.Ui : UIX.S) *)
  | _ ->
      prerr_endline ("Unknown UI '" ^ name ^ "'. Try: notty");
      exit 2

let () =
  (* CLI *)
  let server = ref "" in
  let port = ref 0 in
  let nick = ref "" in
  let realname = ref "" in
  let ui_name = ref "notty" in

  let specl =
    [
      ("--server", Arg.Set_string server, "IRC server hostname (required)");
      ("--port", Arg.Set_int port, "IRC server port (required)");
      ("--nick", Arg.Set_string nick, "IRC nickname (unused for now)");
      ("--realname", Arg.Set_string realname, "IRC realname (unused for now)");
      ("--ui", Arg.Set_string ui_name, "UI adapter (default: notty)");
    ]
  in
  let usage =
    "omni-irc-client --server HOST --port PORT [--ui notty]"
  in
  Arg.parse specl (fun _ -> ()) usage;

  let missing what () = prerr_endline ("missing required arg: " ^ what); exit 2 in
  if !server = "" then missing "--server" ();
  if !port <= 0 then missing "--port" ();

  Lwt_main.run begin
    let cfg : Conn.cfg =
      {
        host = !server;
        port = !port;
        username = None;
        password = None;
        realname = None;
        charset = None;
        tls = false;
        keepalive = false;
      }
    in

    (* In-process queues between UI and network *)
    let to_client_m   : UIX.to_client Lwt_mvar.t   = Lwt_mvar.create_empty () in
    let from_client_m : UIX.from_client Lwt_mvar.t = Lwt_mvar.create_empty () in

    let ui_from_client () = Lwt_mvar.take from_client_m in
    let ui_to_client msg  = Lwt_mvar.put to_client_m msg in

    (* Start UI *)
    let (module UIM : UIX.S) = select_ui !ui_name in
    let ui = UIM.create () in
    let ui_task =
      UIM.run ui ~from_client:ui_from_client ~to_client:ui_to_client
      >>= fun () -> UIM.close ui
    in

    (* Start NET *)
    let net_task =
      Conn.connect cfg >>= fun tconn ->
      Lwt_io.eprintf "[client] TCP connected.\n%!" >>= fun () ->

      let rec rx_loop () =
        let buf = Bytes.create 4096 in
        Tcp.IO.recv tconn buf >>= function
        | 0 ->
            Lwt_mvar.put from_client_m UIX.ClientClosed
        | n ->
            Lwt_mvar.put from_client_m (UIX.ClientRxChunk (Bytes.sub buf 0 n))
            >>= rx_loop
      in

      let rec tx_loop () =
        Lwt_mvar.take to_client_m >>= function
        | UIX.UiSendRaw b ->
            Tcp.IO.send tconn ~off:0 ~len:(Bytes.length b) b >>= fun _ -> tx_loop ()
        | UIX.UiQuit ->
            Lwt.return_unit
      in

      Lwt.finalize
        (fun () -> Lwt.pick [ rx_loop (); tx_loop () ])
        (fun () ->
          Lwt_io.eprintf "[client] closing TCP connection\n%!" >>= fun () ->
          Lwt.catch (fun () -> Conn.close tconn) (fun _ -> Lwt.return_unit))
    in

    (* Either UI or NET finishing ends the program *)
    Lwt.pick [ ui_task; net_task ]
  end
