(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

(* Canonical variants used by the concrete UIs (e.g., notty). *)
type ui_event =
  | UiConnected
  | ClientInfo of string
  | ClientRxChunk of bytes
  | ClientClosed

type ui_action =
  | UiSendRaw of bytes
  | UiCmd of string * string list
  | UiQuit

(* Compatibility aliases expected by the client/main code. *)
type from_client = ui_event
type to_client   = ui_action

module type S = sig
  type t
  val create : unit -> t

  (** Run the UI until it completes or asks to quit.
      - [from_client] must block until next message from the client
      - [to_client] must enqueue a message to the client *)
  val run :
    t ->
    from_client:(unit -> from_client Lwt.t) ->
    to_client:(to_client -> unit Lwt.t) ->
    unit Lwt.t

  val close : t -> unit Lwt.t
end
