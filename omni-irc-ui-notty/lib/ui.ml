(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
open Lwt.Infix

module Y  = Yojson.Safe
module JU = Yojson.Safe.Util
module UIX = Irc_ui.Ui_intf
module SMap = Map.Make(String)

(* Local alias matching what Notty_lwt.event is in newer Notty *)
type notty_event = [ Notty.Unescape.event | `Resize of int * int ]

(* Local channel-list entry (what the client sends after /LIST) *)
type chan_entry = {
  name      : string;
  num_users : int;
  topic     : string option;
}

(* UI-side channel object (authoritative for the UI) *)
type ui_channel = {
  name   : string; (* wire name like "#ocaml" *)
  topic  : string option;
  users  : string list;
  ops    : string list;
  voices : string list;
}

let max_lines = 5000  (* keep a bounded backlog to avoid OOM with /LIST *)

type t = {
  term      : Notty_lwt.Term.t;
  events    : notty_event Lwt_stream.t;
  lines     : string list ref;  (* oldest -> newest *)
  scroll    : int ref;          (* 0 = bottom *)
  input_buf : Buffer.t;
  rx_acc    : Buffer.t;
  chanlist  : chan_entry array ref;
  channels  : ui_channel SMap.t ref;  (* keyed by wire name *)
  draw_lock : Lwt_mutex.t;            (* serialize Notty draws *)
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
    chanlist = ref [||];
    channels = ref SMap.empty;
    draw_lock = Lwt_mutex.create ();
  }

(* helpers *)

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

(* strip mIRC control codes (0x02, 0x03, 0x0F, 0x16, 0x1D, 0x1F) and color params *)
let strip_mirc_codes (s : string) =
  let len = String.length s in
  let b = Bytes.create len in
  let skip_color_params i =
    (* After \x03, skip up to two digits, optional comma, up to two more digits *)
    let is_digit i = i < len && let c = s.[i] in c >= '0' && c <= '9' in
    let rec eat_digits i n = if n = 0 || not (is_digit i) then i else eat_digits (i+1) (n-1) in
    let i = eat_digits i 2 in
    if i < len && s.[i] = ',' then
      let i = i + 1 in
      eat_digits i 2
    else i
  in
  let rec loop i j =
    if i >= len then Bytes.sub_string b 0 j
    else
      let c = s.[i] in
      let code = Char.code c in
      if code = 0x02 || code = 0x0F || code = 0x16 || code = 0x1D || code = 0x1F then
        loop (i+1) j
      else if code = 0x03 then
        let i' = skip_color_params (i+1) in
        loop i' j
      else (
        Bytes.set b j c;
        loop (i+1) (j+1)
      )
  in
  loop 0 0

(* ensure a string is valid UTF-8; replace invalid bytes with U+FFFD *)
let ensure_valid_utf8 (s : string) : string =
  let len = String.length s in
  let b = Buffer.create (len + 8) in
  let add_repl () = Buffer.add_string b "\xEF\xBF\xBD" in
  let byte i = Char.code s.[i] in
  let is_cont i = i < len && (byte i land 0xC0) = 0x80 in
  let rec go i =
    if i >= len then Buffer.contents b else
    let c = byte i in
    if c < 0x80 then (Buffer.add_char b s.[i]; go (i+1))
    else if c < 0xC2 then (add_repl (); go (i+1)) (* disallow overlong/lead 0x80..0xC1 *)
    else if c < 0xE0 then
      if is_cont (i+1) then (Buffer.add_string b (String.sub s i 2); go (i+2))
      else (add_repl (); go (i+1))
    else if c < 0xF0 then
      if is_cont (i+1) && is_cont (i+2) then (Buffer.add_string b (String.sub s i 3); go (i+3))
      else (add_repl (); go (i+1))
    else if c < 0xF5 then
      if is_cont (i+1) && is_cont (i+2) && is_cont (i+3) then
        (Buffer.add_string b (String.sub s i 4); go (i+4))
      else (add_repl (); go (i+1))
    else (add_repl (); go (i+1))
  in
  go 0

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

let repeat_string (s:string) (n:int) : string =
  let n = max 0 n in
  let b = Buffer.create (n * String.length s) in
  for _ = 1 to n do Buffer.add_string b s done;
  Buffer.contents b

let center_in_width (w:int) (s:string) : string =
  let len = String.length s in
  if len >= w then s
  else String.make ((w - len) / 2) ' ' ^ s

let chop width s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let len = min width (n - i) in
      go (i + len) (String.sub s i len :: acc)
  in
  go 0 []

let word_wrap width (words:string list) : string list =
  let rec emit cur_len cur acc = function
    | [] ->
        let acc = if cur = "" then acc else cur :: acc in
        List.rev acc
    | w :: ws ->
        let wl = String.length w in
        if wl > width then
          (* break long word, but keep current line first *)
          let acc = if cur = "" then acc else cur :: acc in
          let pieces = chop width w in
          emit 0 "" acc (pieces @ ws)
        else if cur = "" then
          emit wl w acc ws
        else if cur_len + 1 + wl <= width then
          emit (cur_len + 1 + wl) (cur ^ " " ^ w) acc ws
        else
          emit wl w (cur :: acc) ws
  in
  emit 0 "" [] words

let boxed_section_lines ~w ~title ~items : string list =
  let w = max 20 w in
  let margin_lr = 2 in
  (* total box width excluding the left/right margin spaces *)
  let box_w = max 12 (w - (2 * margin_lr)) in
  let inner_pad = 1 in
  let content_w = max 1 (box_w - 2 (*borders*) - 2 * inner_pad) in
  let text_items = if items = [] then [ "(none)" ] else items in
  let lines =
    word_wrap content_w text_items
    |> (fun ws -> if ws = [] then [ "" ] else ws)
  in
  let space n = String.make n ' ' in
  let margin = space margin_lr in
  let hline  = repeat_string "─" (box_w - 2) in
  let top    = margin ^ "┌" ^ hline ^ "┐" in
  let body =
    List.map
      (fun l ->
        let pad = space (max 0 (content_w - String.length l)) in
        margin ^ "│" ^ space inner_pad ^ l ^ pad ^ space inner_pad ^ "│")
      lines
  in
  let bottom = margin ^ "└" ^ hline ^ "┘" in
  [
    "";                               (* top vertical margin *)
    center_in_width w (title ^ ":");  (* header centered *)
    top
  ]
  @ body
  @ [ bottom; "" ]                    (* bottom vertical margin *)

let sanitize_line (s : string) =
  (* First remove CR/TAB and map C0 control chars to spaces to avoid weird control effects *)
  let s =
    String.map
      (fun c ->
        match c with
        | '\r' | '\t' -> ' '
        | c when Char.code c < 0x20 -> ' '
        | _ -> c)
      s
  in
  let s = strip_mirc_codes s in
  ensure_valid_utf8 s

let push_output_lines t (ls : string list) =
  let ls = List.map sanitize_line ls in
  let combined = !(t.lines) @ ls in
  let n = List.length combined in
  t.lines :=
    if n <= max_lines then combined
    else drop (n - max_lines) combined

(* drawing (internal) *)

let render t =
  let open Notty in
  let open Notty_lwt in
   Lwt.catch
     (fun () ->
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
    )
    (fun _exn ->
      (* Swallow Notty drawing errors; keep terminal healthy. Optionally log to a ring buffer. *)
      Lwt.return_unit)

let redraw t =
  Lwt_mutex.with_lock t.draw_lock (fun () -> render t)

let push_output_line t s =
  let s = sanitize_line s in
  t.lines := !(t.lines) @ [ s ];
  (* Bound memory: drop oldest lines if exceeding max_lines *)
  let l = !(t.lines) in
  let n = List.length l in
  if n > max_lines then t.lines := drop (n - max_lines) l

(* Now that push_output_line exists, we can safely parse client blobs *)
let handle_client_blob (t : t) (json_line : string) =
  (* Best-effort parse; on error, show a tiny diagnostic rather than exploding *)
  let ok =
    try
      let j = Y.from_string json_line in
      match JU.member "type" j with
      | `String "chanlist" ->
          let entries =
            j |> JU.member "entries" |> JU.to_list |> List.map (fun e ->
              {
                name      = e |> JU.member "name"      |> JU.to_string;
                num_users = e |> JU.member "num_users" |> JU.to_int;
                topic     = e |> JU.member "topic"     |> JU.to_string_option;
              })
            |> Array.of_list
          in
            t.chanlist := entries;
          (* Apply optional view args *)
          let filter_opt = JU.member "filter" j |> JU.to_string_option in
          let limit_opt  = JU.member "limit"  j |> JU.to_int_option in

          (* Build and print a simple table, sorted by users desc *)
          let items =
            entries
            |> Array.to_list
            |> List.sort (fun a b -> compare b.num_users a.num_users)
          in
          let lc s = String.lowercase_ascii s in
          let contains ~needle ~haystack =
            let needle = lc needle and haystack = lc haystack in
            let n = String.length needle and h = String.length haystack in
            if n = 0 then true
            else
              let rec loop i =
                if i + n > h then false
                else if String.sub haystack i n = needle then true
                else loop (i + 1)
              in
              loop 0
          in
          let items =
            match filter_opt with
            | None -> items
            | Some f when f = "" || f = "*" -> items
            | Some f ->
                List.filter (fun (e : chan_entry) ->
                  contains ~needle:f ~haystack:e.name
                ) items
          in
          let items =
            match limit_opt with
            | None -> items
            | Some n when n > 0 ->
                let rec take k xs =
                  match k, xs with
                  | k, _ when k <= 0 -> []
                  | _, [] -> []
                  | k, x::tl -> x :: take (k-1) tl
                in
                take n items
            | _ -> items
          in
          let max_name  =
            List.fold_left (fun m (e : chan_entry) -> max m (String.length e.name)) 7 items in
          let max_users =
            List.fold_left (fun m (e : chan_entry) ->
              max m (String.length (string_of_int e.num_users))) 5 items in
          let header = Printf.sprintf "%-*s  %*s  %s" max_name "Channel" max_users "Users" "Topic" in
          let sep = String.make (max_name + 2 + max_users + 2 + 5) '-' in
          let rows =
            List.map
              (fun (e : chan_entry) ->
                let topic = match e.topic with Some s -> s | None -> "" in
                Printf.sprintf "%-*s  %*d  %s" max_name e.name max_users e.num_users topic)
              items
          in
          push_output_lines t
            (Printf.sprintf "(chanlist: %d entries total)" (Array.length entries)
            :: header :: sep :: rows);
          true
      | `String "channel" ->
          let ch = JU.member "channel" j in
          let to_str_list field =
            match JU.member field ch with
            | `List xs ->
                xs |> List.filter_map (function `String s -> Some s | _ -> None)
            | _ -> []
          in
          (* Basic two-line summary *)
          let topic =
            JU.member "topic" ch |> JU.to_string_option |> Option.value ~default:"(no topic set)"
          in
          let name =
            JU.member "name" ch |> JU.to_string_option |> Option.value ~default:"#(unknown)"
          in
          let users  = to_str_list "users"  in
          let ops    = to_str_list "ops"    in
          let voices = to_str_list "voices" in

          (* Ignore the optional “... / … (N more)” truncation marker when counting *)
          let starts_with s prefix =
            let ls = String.length s and lp = String.length prefix in
            ls >= lp && String.sub s 0 lp = prefix
          in
          let not_ellipsis s =
            not (starts_with s "…" || starts_with s "...")
          in
          let count xs = List.length (List.filter not_ellipsis xs) in
          let n_users  = count users in
          let n_ops    = count ops in
          let n_voice  = count voices in

          push_output_lines t
            [ Printf.sprintf "%s %d online, %d op, %d voice" name n_users n_ops n_voice;
              "Topic " ^ topic ];
          true
      | `String "channels" ->
          let op = JU.member "op" j |> JU.to_string_option |> Option.value ~default:"upsert" in
          let parse_channel (k:string) (v:Yojson.Safe.t) : (string * ui_channel) =
            let get_list name =
              match JU.member name v with
              | `Null -> []
              | x ->
                  JU.to_list x
                  |> List.filter_map (fun e -> match e with `String s -> Some s | _ -> None)
            in
            let name   = (JU.member "name" v |> JU.to_string_option) |> Option.value ~default:k in
            let topic  = JU.member "topic" v |> JU.to_string_option in
            let users  = get_list "users" in
            let ops    = get_list "ops" in
            let voices = get_list "voices" in
            (k, { name; topic; users; ops; voices })
          in
          begin match op with
          | "snapshot" | "upsert" ->
              let assoc =
                match JU.member "channels" j with
                | `Assoc pairs -> pairs
                | _ -> []
              in
              let parsed = List.map (fun (k,v) -> parse_channel k v) assoc in
              if op = "snapshot" then t.channels := SMap.empty;
              List.iter (fun (k,ch) -> t.channels := SMap.add k ch !(t.channels)) parsed;
              let msg =
                match op, parsed with
                | ("upsert", [(_k, ch)]) ->
                    Printf.sprintf "(channel updating: %s)" ch.name
                | _ ->
                    Printf.sprintf "(channel %s: %d item%s)"
                      op (List.length parsed) (if List.length parsed = 1 then "" else "s")
              in
              push_output_line t msg; true
          | "remove" ->
              let names =
                match JU.member "names" j with
                | `List xs ->
                    xs |> List.filter_map (function `String s -> Some s | _ -> None)
                | _ -> []
              in
              List.iter (fun k -> t.channels := SMap.remove k !(t.channels)) names;
              push_output_line t
                (Printf.sprintf "(channels remove: %d item%s)"
                  (List.length names) (if List.length names = 1 then "" else "s"));
              true
          | _ ->
              false
          end
      | _ -> false
  with _ -> false
in
if not ok then push_output_line t "(CLIENT blob ignored: unrecognized or invalid JSON)"

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
  push_output_lines t (List.map strip_cr complete)

let scroll_by t delta =
  let open Notty_lwt in
  let _, h = Term.size t.term in
  let body_h = max 1 (h - 1) in
  let w, _ = Term.size t.term in
  let total = List.length (reflow (max 1 w) !(t.lines)) in
  let max_scroll = max 0 (total - body_h) in
  t.scroll := clamp 0 (!(t.scroll) + delta) max_scroll;
  redraw t

(* main run loop *)

let run t ~from_client ~to_client =
  let open Notty_lwt in


  (* -------- Slash command parsing -------- *)
  let trim (s:string) =
    let n = String.length s in
    let i = ref 0 and j = ref (n - 1) in
    while !i < n && (match s.[!i] with ' ' | '\t' -> true | _ -> false) do incr i done;
    while !j >= !i && (match s.[!j] with ' ' | '\t' -> true | _ -> false) do decr j done;
    if !j < !i then "" else String.sub s !i (!j - !i + 1)
  in

  let split_words (s:string) =
    s |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")
  in

  (* /msg <target> <message with spaces>  -> ("PRIVMSG",[target; message])
    /join <ch>                           -> ("JOIN",[ch])
    /names [ch]                          -> ("NAMES",[ch?])
    /nick <new>                          -> ("NICK",[new])
    /list [substr]                       -> ("GET_LIST",[substr?])
    /raw  <rest of line>                 -> ("RAW",[...])
    /anything-else ...                   -> (UPPERCASE, args) (lets dispatcher decide) *)
  let parse_slash (line:string) : [ `Cmd of string * string list | `Raw of string ] =
    if String.length line > 0 && line.[0] = '/' then (
      let cmdline = trim (String.sub line 1 (String.length line - 1)) in
      if cmdline = "" then `Raw "" else
      match split_words cmdline with
      | [] -> `Raw ""
      | root :: args ->
          let r = String.lowercase_ascii root in
          begin match r with
          | "join" | "j" ->
              `Cmd ("JOIN", args)
          | "names" ->
              `Cmd ("NAMES", args)
          | "nick" ->
              `Cmd ("NICK", args)
          | "msg" | "privmsg" ->
              (match args with
              | tgt :: rest ->
                  let msg = String.concat " " rest in
                  `Cmd ("PRIVMSG", [tgt; msg])
              | [] -> `Cmd ("PRIVMSG", []))
          | "list" ->
              `Cmd ("GET_LIST", args)
          | "raw" ->
              `Cmd ("RAW", args)
          | other ->
              `Cmd (String.uppercase_ascii other, args)
          end
    ) else `Raw line
  in

  let request_quit () : unit Lwt.t =
    to_client UIX.UiQuit
  in

  let rec ui_loop () =
    Lwt_stream.next t.events >>= function
    | `Key (`Enter, _) ->
        let line = Buffer.contents t.input_buf in
        Buffer.clear t.input_buf;
        if line <> "" then push_output_line t ("> " ^ line);
        redraw t >>= fun () ->
          (match parse_slash line with
            | `Cmd (key, args) ->
              to_client (UIX.UiCmd (key, args))
            | `Raw s ->
              to_client (UIX.UiSendRaw (Bytes.of_string (s ^ "\r\n"))))
        >>= ui_loop
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
      (* Intercept "CLIENT <json>" control lines *)
      let prefix = "CLIENT " in
      if String.length s >= String.length prefix
          && String.sub s 0 (String.length prefix) = prefix
      then (
        let payload = String.sub s (String.length prefix) (String.length s - String.length prefix) in
        handle_client_blob t payload;
        redraw t >>= from_client_loop
      ) else (
        push_output_line t s;
        redraw t >>= from_client_loop
      )
  | UIX.ClientRxChunk b ->
      feed_chunk t (Bytes.unsafe_to_string b);
      redraw t >>= from_client_loop
  in

  Lwt.finalize
    (fun () -> redraw t >>= fun () -> Lwt.join [ ui_loop (); from_client_loop () ])
    (fun () -> Term.release t.term)

let close _t = Lwt.return_unit
