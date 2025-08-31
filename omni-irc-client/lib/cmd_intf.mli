(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

(** 1-key -> 1-handler dispatcher; no runtime registration; fully replaceable. *)
module type S = sig
  type ctx
  type t

  val create : unit -> t

  val dispatch :
    t -> ctx -> key:Cmd_key.t -> args:string list -> unit Lwt.t

  val dispatch_async :
    t -> ctx -> key:Cmd_key.t -> args:string list -> unit
end
