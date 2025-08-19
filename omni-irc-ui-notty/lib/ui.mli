(** Minimal Notty-Lwt UI that attaches to an AF_UNIX socket and echoes bytes. *)

val run :
  socket_path:string ->
  unit Lwt.t
