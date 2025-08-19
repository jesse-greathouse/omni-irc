open Lwt.Infix

(* AF_UNIX side for the local UI attachment *)
module U   = Irc_io_unixsock.Unixsock_io
module UIO = U.IO

(* TCP dialer via connector: Tcp IO + connector adapter *)
module Tcp = struct
  module Endpoint = Irc_io_tcp.Tcp_io.Endpoint
  module IO       = Irc_io_tcp.Tcp_io.IO
end
module Conn = Irc_conn_tcp.Connector_tcp.Make(Tcp)

(* CLI *)
let () =
  let server   = ref "" in
  let port     = ref 0 in
  let nick     = ref "" in
  let realname = ref "" in
  let socket   = ref "/tmp/omni-irc.sock" in
  let ui_cmd   = ref "" in

  let specl = [
    ("--server",   Arg.Set_string server,   "IRC server hostname (required)");
    ("--port",     Arg.Set_int    port,     "IRC server port (required)");
    ("--nick",     Arg.Set_string nick,     "IRC nickname (unused for now)");
    ("--realname", Arg.Set_string realname, "IRC realname (unused for now)");
    ("--socket",   Arg.Set_string socket,   "AF_UNIX socket path (default /tmp/omni-irc.sock)");
    ("--ui",       Arg.Set_string ui_cmd,   "UI program to spawn (e.g. omni-irc-tui)");
  ] in
  let usage =
    "omni-irc-client --server HOST --port PORT --nick N --realname R --socket PATH [--ui CMD]"
  in
  Arg.parse specl (fun _ -> ()) usage;

  let missing what () =
    prerr_endline ("missing required arg: " ^ what);
    exit 2
  in
  if !server = "" then missing "--server" ();
  if !port <= 0 then missing "--port" ();

  (* Pipes bridging AF_UNIX IO <-> TCP fd (Tcp.IO.t = Lwt_unix.file_descr) *)
  let rec pipe_ui_to_tcp ~(src : UIO.t) ~(dst : Tcp.IO.t) =
    let buf = Bytes.create 4096 in
    UIO.recv src buf >>= function
    | 0 -> Lwt_io.eprintf "[pipe] unix->tcp EOF\n%!"
    | n ->
      Lwt_io.eprintf "[pipe] unix->tcp %d bytes\n%!" n >>= fun () ->
      Tcp.IO.send dst ~off:0 ~len:n buf >>= fun _ ->
      pipe_ui_to_tcp ~src ~dst
  in

  let rec pipe_tcp_to_ui ~(src : Tcp.IO.t) ~(dst : UIO.t) =
    let buf = Bytes.create 4096 in
    Tcp.IO.recv src buf >>= function
    | 0 -> Lwt_io.eprintf "[pipe] tcp->unix EOF\n%!"
    | n ->
      Lwt_io.eprintf "[pipe] tcp->unix %d bytes\n%!" n >>= fun () ->
      UIO.send dst ~off:0 ~len:n buf >>= fun _ ->
      pipe_tcp_to_ui ~src ~dst
  in

  (* Once a single AF_UNIX client attaches, bridge it to a TCP connection *)
  let handler (cfd : UIO.t) =
    let cfg : Conn.cfg = {
      host      = !server;
      port      = !port;
      username  = None;
      password  = None;
      realname  = None;
      charset   = None;
      tls       = false;     (* flip to true when TLS is implemented in IO *)
      keepalive = false;     (* future: use in a higher layer for PINGs *)
    } in
    Lwt_io.eprintf "[client] AF_UNIX client attached; dialing %s:%d via connector...\n%!"
      cfg.host cfg.port
    >>= fun () ->
    Conn.connect cfg >>= fun tconn ->
    Lwt_io.eprintf "[client] TCP connected.\n%!" >>= fun () ->
    Lwt.finalize
      (fun () ->
        Lwt.pick [
          pipe_ui_to_tcp ~src:cfd ~dst:tconn;
          pipe_tcp_to_ui ~src:tconn ~dst:cfd;
        ])
      (fun () ->
        Lwt_io.eprintf "[client] closing TCP connection\n%!" >>= fun () ->
        Lwt.catch (fun () -> Conn.close tconn) (fun _ -> Lwt.return_unit))
  in

  (* Optionally spawn the UI with OMNI_IRC_SOCKET exported *)
  let spawn_ui_if_requested () =
    if !ui_cmd = "" then Lwt.return_unit
    else
      let env =
        let cur = Array.to_list (Unix.environment ()) in
        let cur = List.filter (fun s -> not (String.starts_with ~prefix:"OMNI_IRC_SOCKET=" s)) cur in
        Array.of_list (("OMNI_IRC_SOCKET=" ^ !socket) :: cur)
      in
      let cmd = (!ui_cmd, [| !ui_cmd |]) in
      let proc = Lwt_process.open_process_none ~env cmd in
      Lwt.async (fun () -> proc#status >|= fun _ -> ());
      Lwt.return_unit
  in

  Lwt_main.run begin
    U.serve_once ~path:!socket ~perms:0o600 handler >>= fun srv ->
    spawn_ui_if_requested () >>= fun () ->
    Lwt_io.printf "[client] socket: %s  ->  TCP %s:%d (via connector)\n%!"
      !socket !server !port
    >>= fun () ->
    (* Wait for the accept/bridge to finish (client quits or connection closes) *)
    U.wait srv
  end
