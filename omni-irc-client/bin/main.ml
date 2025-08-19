open Lwt.Infix

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

  let module U  = Irc_io_unixsock.Unixsock_io in
  let module IO = U.IO in

  (* Resolve and connect TCP to IRC *)
  let connect_tcp host port =
    let open Unix in
    let hints = [ AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM ] in
    Lwt_unix.getaddrinfo host (string_of_int port) hints >>= function
    | [] -> Lwt.fail_with ("no addrinfo for " ^ host)
    | ai :: _ ->
      let fd = Lwt_unix.socket ai.ai_family ai.ai_socktype ai.ai_protocol in
      Lwt_unix.connect fd ai.ai_addr >|= fun () -> fd
  in

  (* Pipes bridging AF_UNIX IO <-> TCP fd *)
  let rec pipe_io_to_fd ~(src : IO.t) ~(dst : Lwt_unix.file_descr) =
    let buf = Bytes.create 4096 in
    IO.recv src buf >>= function
    | 0 -> Lwt_io.eprintf "[pipe] unix->tcp EOF\n%!"
    | n ->
      Lwt_io.eprintf "[pipe] unix->tcp %d bytes\n%!" n >>= fun () ->
      Lwt_unix.write dst buf 0 n >>= fun _ ->
      pipe_io_to_fd ~src ~dst
  in

  let rec pipe_fd_to_io ~(src : Lwt_unix.file_descr) ~(dst : IO.t) =
    let buf = Bytes.create 4096 in
    Lwt_unix.read src buf 0 4096 >>= function
    | 0 -> Lwt_io.eprintf "[pipe] tcp->unix EOF\n%!"
    | n ->
      Lwt_io.eprintf "[pipe] tcp->unix %d bytes\n%!" n >>= fun () ->
      IO.send dst ~off:0 ~len:n buf >>= fun _ ->
      pipe_fd_to_io ~src ~dst
  in

  (* Once a single AF_UNIX client attaches, bridge it to the TCP connection *)
  let handler (cfd : IO.t) =
    Lwt_io.eprintf "[client] AF_UNIX client attached; connecting TCP %s:%d...\n%!"
      !server !port
    >>= fun () ->
    connect_tcp !server !port >>= fun tfd ->
    Lwt_io.eprintf "[client] TCP connected.\n%!" >>= fun () ->
    Lwt.finalize
      (fun () ->
        Lwt.pick [
          pipe_io_to_fd ~src:cfd ~dst:tfd;
          pipe_fd_to_io ~src:tfd ~dst:cfd;
        ])
      (fun () ->
        Lwt_io.eprintf "[client] closing TCP fd\n%!" >>= fun () ->
        Lwt.catch (fun () -> Lwt_unix.close tfd) (fun _ -> Lwt.return_unit))
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
    Lwt_io.printf "[client] socket: %s  ->  TCP %s:%d\n%!" !socket !server !port
    >>= fun () ->
    (* Wait for the accept/bridge to finish (client quits or connection closes) *)
    U.wait srv
  end
