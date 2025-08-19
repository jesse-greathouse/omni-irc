open Lwt.Infix

let run ~socket_path =
  let open Notty in
  let open Notty_lwt in

  let term   = Term.create () in
  let events = Term.events term in

  let input_mvar   = Lwt_mvar.create_empty () in
  let output_chunks = ref [] in

  (* Replace control chars except printable ASCII; drop CR, expand TAB, map others *)
  let sanitize_line (s : string) =
    String.map (fun c ->
      match c with
      | '\r' -> ' '                (* drop CR effect *)
      | '\t' -> ' '                (* keep it simple; no tabs *)
      | c when Char.code c < 0x20 -> '?'  (* visualize other controls *)
      | _ -> c
    ) s
  in

  let lines_of_chunk (s : string) : string list =
    s
    |> String.split_on_char '\n'
    |> List.map sanitize_line
  in

  let footer_img =
    I.vcat [
      I.string A.empty "--------------------------------";
      I.string A.empty "enter=\\r\\n       q/esc/ctrl-c=quit";
    ]
  in

  let redraw () =
    let body_lines =
      (* newest last, top-to-bottom *)
      !output_chunks |> List.rev |> List.concat_map lines_of_chunk
    in
    let body_imgs =
      match body_lines with
      | [] -> [ I.string A.empty "(no output yet)" ]
      | xs -> List.map (fun s -> I.string A.empty s) xs
    in
    let img = I.vcat (body_imgs @ [ footer_img ]) in
    Term.image term img
  in

  (* Connect to AF_UNIX and pump both ways *)
  let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Lwt_unix.connect fd (Unix.ADDR_UNIX socket_path) >>= fun () ->

  let close_fd_safely () =
    Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)
  in

  (* UI loop: send bytes to writer via input_mvar, and support quit *)
  let rec ui_loop () =
    Lwt_stream.next events >>= function
    | `Key (`Enter, _) ->
        Lwt_mvar.put input_mvar (Bytes.of_string "\r\n") >>= ui_loop
    | `Key (`ASCII c, mods) when List.mem `Ctrl mods && (c = 'c' || c = 'C') ->
        close_fd_safely ()
    | `Key (`ASCII 'q', _) | `Key (`ASCII 'Q', _) | `Key (`Escape, _) ->
        close_fd_safely ()
    | `Key (`ASCII c, _) ->
        Lwt_mvar.put input_mvar (Bytes.of_string (String.make 1 c)) >>= ui_loop
    | `Resize _ ->
        redraw () >>= ui_loop
    | `Mouse _ | `Paste _ | `Key _ ->
        ui_loop ()
  in

  let rec reader () =
    let buf = Bytes.create 4096 in
    Lwt_unix.read fd buf 0 4096 >>= function
    | 0 -> Lwt.return_unit
    | n ->
        output_chunks := (Bytes.sub_string buf 0 n) :: !output_chunks;
        redraw () >>= reader
  in

  let rec writer () =
    Lwt_mvar.take input_mvar >>= fun b ->
    Lwt_unix.write fd b 0 (Bytes.length b) >>= fun _ ->
    writer ()
  in

  (* initial paint *)
  redraw () >>= fun () ->
  Lwt.join [ ui_loop (); reader (); writer () ]
