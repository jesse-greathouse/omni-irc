open Lwt.Infix

(* Expose the concrete [t] so callers can pass a [Lwt_unix.file_descr] as [IO.t]. *)
module IO : Irc_sig.Io.S with type t = Lwt_unix.file_descr and type endpoint = string = struct
  type t = Lwt_unix.file_descr
  type endpoint = string (* socket path *)

  let connect (path : endpoint) =
    let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Lwt_unix.connect fd (Unix.ADDR_UNIX path) >|= fun () -> fd

  let recv fd buf = Lwt_unix.read fd buf 0 (Bytes.length buf)

  let send fd ?(off = 0) ?len buf =
    let len = match len with Some l -> l | None -> Bytes.length buf - off in
    Lwt_unix.write fd buf off len

  let close fd = Lwt_unix.close fd
end

type server = { fd : Lwt_unix.file_descr; path : string; stop : unit Lwt.u; wait : unit Lwt.t }

let unlink_safely path =
  Lwt.catch
    (fun () -> Lwt_unix.unlink path)
    (function Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit | _ -> Lwt.return_unit)

let serve_once ~path ~perms (k : IO.t -> unit Lwt.t) =
  (* Fail if exists; otherwise proceed *)
  Lwt.catch
    (fun () ->
      Lwt_unix.stat path >>= fun _ -> Lwt.fail_with ("omni-irc-io-unixsock: path exists: " ^ path))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
          let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
          let addr = Unix.ADDR_UNIX path in
          Lwt_unix.bind fd addr >>= fun () ->
          Lwt_unix.chmod path perms >>= fun () ->
          Lwt_unix.listen fd 1;
          let wait, stop = Lwt.wait () in
          let accept_once () =
            Lwt_unix.accept fd >>= fun (cfd, _) ->
            Lwt_unix.close fd >>= fun () ->
            k cfd >>= fun () -> IO.close cfd
          in
          let joined = Lwt.finalize accept_once (fun () -> unlink_safely path) in
          Lwt.return { fd; path; stop; wait = Lwt.join [ wait; joined ] }
      | exn -> Lwt.fail exn)

let shutdown t =
  Lwt.wakeup_later t.stop ();
  Lwt.catch (fun () -> Lwt_unix.close t.fd) (fun _ -> Lwt.return_unit) >>= fun () ->
  unlink_safely t.path >>= fun () -> t.wait

let wait t = t.wait
