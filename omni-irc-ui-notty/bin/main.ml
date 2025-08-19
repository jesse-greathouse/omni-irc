let () =
  let socket =
    Sys.getenv_opt "OMNI_IRC_SOCKET"
    |> Option.value ~default:"/tmp/omni-irc.sock"
  in
  Lwt_main.run (Irc_ui_notty.Ui.run ~socket_path:socket)
