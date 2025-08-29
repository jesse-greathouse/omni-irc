(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
type to_client =
  | UiSendRaw of bytes        (** user typed a line; send verbatim to IRC *)
  | UiQuit                    (** user requested quit *)

type from_client =
  | ClientRxChunk of bytes    (** network bytes from IRC server *)
  | ClientClosed              (** TCP closed / final notice *)
  | ClientInfo of string      (** optional status/log lines *)

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
