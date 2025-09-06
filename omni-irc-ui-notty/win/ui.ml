(* omni-irc-ui-notty/lib/win/ui.ml *)
open Lwt.Infix
module UIX = Irc_ui.Ui_intf

type t = unit
let create () = ()

let rec drain from_client =
  from_client () >>= function
  | UIX.ClientClosed -> Lwt.return_unit
  | _ -> drain from_client

let run _ ~from_client ~to_client =
  (* kick off the client side since this is a stub *)
  to_client (UIX.UiCmd ("CONNECT", [])) >>= fun () ->
  drain from_client

let close _ = Lwt.return_unit
