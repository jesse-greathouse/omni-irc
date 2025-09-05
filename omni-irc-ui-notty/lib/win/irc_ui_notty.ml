(* Windows stub: satisfy Irc_ui_notty.Ui *)
open Lwt.Infix
module UIX = Irc_ui.Ui_intf

module Ui : UIX.S = struct
  type t = unit
  let create () = ()
  let run _ ~from_client:_ ~to_client:_ =
    Lwt_io.eprintl "Notty UI is unavailable on Windows; use --ui headless (loopback)." >>= fun () ->
    Lwt.return_unit
  let close _ = Lwt.return_unit
end
