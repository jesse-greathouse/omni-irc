(* Windows stub: satisfy references to Irc_ui_headless.Ui *)
open Lwt.Infix
module UIX = Irc_ui.Ui_intf

module Ui : UIX.S = struct
  type t = unit
  let create () = ()
  let run _ ~from_client:_ ~to_client:_ =
    (* Never used on Windows; if it is, just tell the user and exit quietly. *)
    Lwt_io.eprintl "headless UI is unavailable on Windows (use /ui loopback)" >>= fun () ->
    Lwt.return_unit
  let close _ = Lwt.return_unit
end
