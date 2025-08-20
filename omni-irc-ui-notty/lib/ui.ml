open Lwt.Infix

(* Pull in the UI interface *)
module UIX = Irc_ui.Ui_intf

(* Local alias matching what Notty_lwt.event is in newer Notty *)
type notty_event = [ Notty.Unescape.event | `Resize of int * int ]

type t = {
  term      : Notty_lwt.Term.t;
  events    : notty_event Lwt_stream.t;
  lines     : string list ref;  (* oldest -> newest *)
  scroll    : int ref;          (* 0 = bottom *)
  input_buf : Buffer.t;
  rx_acc    : Buffer.t;
}

let create () =
  let open Notty_lwt in
  let term = Term.create () in
  let events = Term.events term in
  {
    term; events;
    lines = ref [];
    scroll = ref 0;
    input_buf = Buffer.create 256;
    rx_acc = Buffer.create 4096;
  }

(* ---------- helpers ---------- *)

let append_uchar_utf8 (buf : Buffer.t) (u : Uchar.t) =
  let n = Uchar.to_int u in
  let add c = Buffer.add_char buf (Char.chr c) in
  if n < 0x80 then add n
  else if n < 0x800 then (add (0xC0 lor (n lsr 6)); add (0x80 lor (n land 0x3F)))
  else if n < 0x10000 then (
    add (0xE0 lor (n lsr 12));
    add (0x80 lor ((n lsr 6) land 0x3F));
    add (0x80 lor (n land 0x3F)))
  else (
    add (0xF0 lor (n lsr 18));
    add (0x80 lor ((n lsr 12) land 0x3F));
    add (0x80 lor ((n lsr 6) land 0x3F));
    add (0x80 lor (n land 0x3F)))

let sanitize_line (s : string) =
  String.map
    (fun c ->
      match c with
      | '\r' | '\t' -> ' '
      | c when Char.code c < 0x20 -> '?'
      | _ -> c)
    s

let endswith (s:string) (ch:char) =
  let n = String.length s in
  n > 0 && s.[n - 1] = ch

let strip_cr s = if endswith s '\r' then String.sub s 0 (String.length s - 1) else s

let wrap_line width s =
  if width <= 0 then [ "" ]
  else
    let n = String.length s in
    let rec go i acc =
      if i >= n then List.rev acc
      else
        let len = min width (n - i) in
        go (i + len) (String.sub s i len :: acc)
    in
    go 0 []

let take n xs =
  let rec loop n xs acc =
    match (n, xs) with
    | 0, _ | _, [] -> List.rev acc
    | n, x :: tl -> loop (n - 1) tl (x :: acc)
  in
  loop n xs []

let drop n xs =
  let rec loop n xs =
    match (n, xs) with
    | 0, xs -> xs
    | _, [] -> []
    | n, _ :: tl -> loop (n - 1) tl
  in
  loop n xs

let reflow width (lines:string list) = List.concat_map (wrap_line width) lines

let clamp a x b = max a (min x b)

(* ---------- drawing ---------- *)

let redraw t =
  let open Notty in
  let open Notty_lwt in
  let w, h = Term.size t.term in
  let w = max 1 w in
  let h = max 2 h in
  let body_h = h - 1 in

  let wrapped = reflow w !(t.lines) in
  let total = List.length wrapped in
  let max_scroll = max 0 (total - body_h) in
  t.scroll := clamp 0 !(t.scroll) max_scroll;

  let start = max 0 (total - body_h - !(t.scroll)) in
  let visible = take body_h (drop start wrapped) in

  let body_img =
    match visible with
    | [] -> I.string A.empty "(no output yet)"
    | _ ->
      I.vcat
        (List.map
            (fun s ->
              let s = if String.length s > w then String.sub s 0 w else s in
              I.string A.empty s)
            visible)
  in

  let prompt = "> " in
  let input = Buffer.contents t.input_buf in
  let room = max 1 (w - String.length prompt) in
  let tail =
    if String.length input <= room then input
    else String.sub input (String.length input - room) room
  in
  let pad = String.make (max 0 (room - String.length tail)) ' ' in
  let input_img =
    I.hcat [ I.string Notty.A.(st bold) prompt; I.string A.empty tail; I.string A.empty pad ]
  in

  let img = I.vcat [ body_img; input_img ] in
  Term.image t.term img >>= fun () ->
  let cx = min (w - 1) (String.length prompt + String.length tail) in
  Term.cursor t.term (Some (cx, h - 1))

let push_output_line t s =
  t.lines := !(t.lines) @ [ sanitize_line s ];
  Lwt.async (fun () -> redraw t)

let feed_chunk t (chunk : string) =
  Buffer.add_string t.rx_acc chunk;
  let s = Buffer.contents t.rx_acc in
  let parts = String.split_on_char '\n' s in
  let init, last =
    match List.rev parts with [] -> ([], "") | last :: rev_init -> (List.rev rev_init, last)
  in
  let ends_with_nl = endswith s '\n' in
  let complete = if ends_with_nl then init @ [ last ] else init in
  let rest = if ends_with_nl then "" else last in
  Buffer.clear t.rx_acc;
  Buffer.add_string t.rx_acc rest;
  List.iter (fun l -> push_output_line t (strip_cr l)) complete

let scroll_by t delta =
  let open Notty_lwt in
  let _, h = Term.size t.term in
  let body_h = max 1 (h - 1) in
  let w, _ = Term.size t.term in
  let total = List.length (reflow (max 1 w) !(t.lines)) in
  let max_scroll = max 0 (total - body_h) in
  t.scroll := clamp 0 (!(t.scroll) + delta) max_scroll;
  redraw t

(* ---------- main run loop ---------- *)

let run t ~from_client ~to_client =
  let open Notty_lwt in

  let request_quit () : unit Lwt.t =
    to_client UIX.UiQuit
  in

  let rec ui_loop () =
    Lwt_stream.next t.events >>= function
    | `Key (`Enter, _) ->
        let line = Buffer.contents t.input_buf in
        Buffer.clear t.input_buf;
        if line <> "" then push_output_line t ("> " ^ line);
        to_client (UIX.UiSendRaw (Bytes.of_string (line ^ "\r\n"))) >>= ui_loop
    | `Key (`Backspace, _) ->
        let n = Buffer.length t.input_buf in
        if n > 0 then Buffer.truncate t.input_buf (n - 1);
        redraw t >>= ui_loop
    | `Key (`Escape, _) | `Key (`ASCII 'q', _) | `Key (`ASCII 'Q', _) ->
        request_quit ()
    | `Key (`ASCII c, mods) when List.mem `Ctrl mods && (c = 'c' || c = 'C') ->
        request_quit ()
    | `Key (`Uchar u, mods)
      when List.mem `Ctrl mods
            && (Uchar.to_int u = Char.code 'c'
                || Uchar.to_int u = Char.code 'C'
                || Uchar.to_int u = 0x03) ->
        request_quit ()
    | `Key (`ASCII c, _) ->
        Buffer.add_char t.input_buf c;
        redraw t >>= ui_loop
    | `Key (`Uchar u, _) ->
        append_uchar_utf8 t.input_buf u;
        redraw t >>= ui_loop
    | `Paste _ ->
        ui_loop ()
    | `Key (`Arrow `Up, _) -> scroll_by t 1   >>= ui_loop
    | `Key (`Arrow `Down, _) -> scroll_by t (-1) >>= ui_loop
    | `Resize _ -> redraw t >>= ui_loop
    | `Mouse _ | `Key _ -> ui_loop ()
  in

  let rec from_client_loop () =
    from_client () >>= function
    | UIX.ClientClosed ->
        push_output_line t "(connection closed)";
        redraw t
    | UIX.ClientInfo s ->
        push_output_line t s;
        redraw t >>= from_client_loop
    | UIX.ClientRxChunk b ->
        feed_chunk t (Bytes.unsafe_to_string b);
        redraw t >>= from_client_loop
  in

  Lwt.finalize
    (fun () -> redraw t >>= fun () -> Lwt.join [ ui_loop (); from_client_loop () ])
    (fun () -> Term.release t.term)

let close _t = Lwt.return_unit
