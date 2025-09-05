(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
open Lwt.Infix

module UIX = Irc_ui.Ui_intf

module Ui : UIX.S = struct
  type t = {
    mutable listening : Lwt_unix.file_descr option;
    mutable active    : Lwt_unix.file_descr option;
    mutable closed    : bool;
    path              : string;
  }

  let getenv k = try Some (Sys.getenv k) with Not_found -> None

  let default_socket_path () =
    (* Fallback if main didn’t set OMNI_IRC_SOCKET *)
    match getenv "OMNI_IRC_SOCKET" with
    | Some p -> p
    | None   -> "/tmp/omni/omni.sock"

  let ensure_dir d =
    if Sys.file_exists d then ()
    else Unix.mkdir d 0o777

  let unlink_if_exists p =
    match (try Some (Unix.stat p) with _ -> None) with
    | Some _ -> (try Unix.unlink p with _ -> ())
    | None -> ()

  let create () =
    let path = default_socket_path () in
    { listening=None; active=None; closed=false; path }

  let rec write_all fd buf off len =
    if len = 0 then Lwt.return_unit else
    Lwt_unix.write fd buf off len >>= fun n ->
    write_all fd buf (off+n) (len-n)

  let send_line fd (s:string) =
    let s = s ^ "\n" in
    write_all fd (Bytes.unsafe_of_string s) 0 (String.length s)

  let send_bytes fd (b:bytes) =
    write_all fd b 0 (Bytes.length b)

  (* Stream socket incoming data into \n-delimited lines. *)
  let socket_read_lines fd push =
    let buf = Bytes.create 4096 in
    let acc = Buffer.create 4096 in
    let rec loop () =
      Lwt_unix.read fd buf 0 (Bytes.length buf) >>= function
      | 0 -> Lwt.return_unit
      | n ->
          Buffer.add_subbytes acc buf 0 n;
          let s = Buffer.contents acc in
          let rec drain start =
            match String.index_from_opt s start '\n' with
            | None ->
                if start > 0 then (
                  Buffer.clear acc;
                  Buffer.add_substring acc s start (String.length s - start)
                );
                Lwt.return_unit
            | Some i ->
                let line =
                  if i > 0 && s.[i-1] = '\r'
                  then String.sub s start (i - start - 1)
                  else String.sub s start (i - start)
                in
                push line >>= fun () ->
                drain (i+1)
          in
          drain 0 >>= loop
    in
    Lwt.catch loop (fun _ -> Lwt.return_unit)

  let accept_once t =
    let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    t.listening <- Some sock;
    let dir = Filename.dirname t.path in
    ensure_dir dir;
    unlink_if_exists t.path;
    Lwt_unix.bind sock (Unix.ADDR_UNIX t.path) >>= fun () ->
    Lwt_unix.listen sock 1;
    Lwt_unix.accept sock >>= fun (fd, _addr) ->
    t.active <- Some fd;
    Lwt.return fd

  let close_active t =
    match t.active with
    | None -> Lwt.return_unit
    | Some fd ->
        t.active <- None;
        Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

  let close_listening t =
    match t.listening with
    | None -> Lwt.return_unit
    | Some fd ->
        t.listening <- None;
        Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

  let run (t:t)
      ~(from_client : unit -> UIX.from_client Lwt.t)
      ~(to_client   : UIX.to_client -> unit Lwt.t)
    =
    (* Accept exactly one frontend; when it disconnects, stop. *)
    if t.closed then Lwt.return_unit else
    accept_once t >>= fun fd ->

    to_client (UIX.UiCmd ("CONNECT", [])) >>= fun () ->

    let out_loop =
      let rec pump () =
        from_client () >>= function
        | UIX.UiConnected     -> pump ()
        | UIX.ClientInfo s    -> send_line fd s >>= pump
        | UIX.ClientRxChunk b -> send_bytes fd b >>= pump
        | UIX.ClientClosed    -> send_line fd "CLIENT_CLOSED" >>= pump
      in
      Lwt.catch pump (fun _ -> Lwt.return_unit)
    in

    let in_loop =
      let push line =
        match Irc_ui.Command.parse line with
        | `Cmd (key, args) -> to_client (UIX.UiCmd (key, args))
        | `Raw s ->
            let raw = Bytes.of_string (s ^ "\r\n") in
            to_client (UIX.UiSendRaw raw)
      in
      socket_read_lines fd push
    in

    (* When either side ends (e.g. frontend disconnects), stop serving. *)
    Lwt.pick [out_loop; in_loop] >>= fun () ->
    (* Close sockets and return (do NOT loop back to accept again). *)
    close_active t >>= fun () ->
    close_listening t

  let close t =
    t.closed <- true;
    close_active t >>= fun () ->
    close_listening t >>= fun () ->
    (try Unix.unlink t.path with _ -> ());
    Lwt.return_unit
end
