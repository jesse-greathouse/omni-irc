(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
open Lwt.Infix

module Y      = Yojson.Safe
module P      = Irc_engine.Parser
module UIX    = Irc_ui.Ui_intf
module Conn   = Irc_conn.Connector
module Engine = Irc_engine.Engine.Make(P)

module User          = Model_user
module Channel       = Model_channel
module Channel_list  = Model_channel_list

module type CONN = sig
  type conn
  val connect : Conn.cfg -> conn Lwt.t
  val recv    : conn -> bytes -> int Lwt.t
  val send    : conn -> ?off:int -> ?len:int -> bytes -> int Lwt.t
  val close   : conn -> unit Lwt.t
end

module type CMD = sig
  type ctx
  type t
  val create : unit -> t
  val dispatch : t -> ctx -> key:Cmd_key.t -> args:string list -> unit Lwt.t
  val dispatch_async : t -> ctx -> key:Cmd_key.t -> args:string list -> unit
end

type opts = {
  nick           : string option;
  realname       : string option;
  alt_nicks      : string list;
  sasl_plain     : (string * string) option;  (* username, password *)
  require_sasl   : bool;
  reg_timeout_s  : float;
}

type boxed =
  | B : (module CONN with type conn = 'c) * 'c -> boxed

module SMap = Map.Make(String)
module CSet = Set.Make(String) 

type t = {
  cfg               : Conn.cfg;
  engine            : t Engine.t;
  ui_mod            : (module UIX.S);
  c_mod             : (module CONN);
  mutable c         : boxed option;
  to_client_m       : UIX.to_client Lwt_mvar.t;
  from_client_m     : UIX.from_client Lwt_mvar.t;
  acc               : Engine.Acc.t;
  opts              : opts;
  mutable running   : bool;
  cmd_pack          : cmd_pack;
  on_connect        : (t -> unit Lwt.t) list ref;
  mutable channels  : Channel.t SMap.t;
  mutable chanlist  : Channel_list.t;
  mutable users     : User.t SMap.t;
  mutable self_user : User.t option;

  (* time-gating for LIST *)
  mutable last_list_refresh : float option;
  mutable last_list_args    : (string option * int option) option;
  mutable names_refreshing  : CSet.t;
  mutable whois_last_ts     : float SMap.t;

  (* --- handshake state --- *)
  mutable registered        : bool;
  mutable cap_active        : bool;
  mutable sasl_in_progress  : bool;
  mutable nick_cycle        : string list;   (* head = current candidate *)
  mutable reg_timer         : unit Lwt.t option;
  mutable was_connected     : bool;
}
and cmd_pack = Pack : (module CMD with type ctx = t and type t = 'a) * 'a -> cmd_pack

(* Make the user key helper available before first use. *)
let user_key_of_nick = User.key_of_nick

type client_ctx = t

(* re-export so Client.Cmd_key works *)
module Cmd_key = Cmd_key

let on_connect (t : t) (f : t -> unit Lwt.t) =
  t.on_connect := f :: !(t.on_connect)

let with_conn (t : t) (f : boxed -> 'r Lwt.t) : 'r Lwt.t =
  match t.c with None -> Lwt.fail_with "client: not connected" | Some b -> f b

let send_raw t line =
  with_conn t (fun (B ((module C), conn)) ->
    let len = String.length line in
    C.send conn ~off:0 ~len (Bytes.of_string line) >>= fun _ -> Lwt.return_unit)

(* channels helpers *)
let channel_key_of s = Channel.key_of_name s

let channel_find t name =
  SMap.find_opt (channel_key_of name) t.channels

let channel_ensure t name =
  let k = channel_key_of name in
  match SMap.find_opt k t.channels with
  | Some ch -> ch
  | None ->
      let ch = Channel.make ~name:k in
      t.channels <- SMap.add k ch t.channels;
      ch

let push_from_client (t : t) (ev : UIX.from_client) : unit Lwt.t =
  Lwt_mvar.put t.from_client_m ev

let notify t s = push_from_client t (UIX.ClientInfo s)
let quit   t   = send_raw t "QUIT :bye\r\n"

(* Pretty print a channel list snapshot into the UI (one line per channel). *)
let emit_channel_list_dump ?filter ?limit (t : t) : unit Lwt.t =
  let open Channel_list in
  let entries_all : entry list =
    Map.bindings t.chanlist |> List.map snd
    |> List.sort (fun a b -> Stdlib.compare b.num_users a.num_users)
  in
  let wire_name (e:entry) = wire_of_name e.name in
  let lc s = String.lowercase_ascii s in
  let contains ~needle ~haystack =
    let needle = lc needle and haystack = lc haystack in
    let n = String.length needle and h = String.length haystack in
    if n = 0 then true else
    let rec loop i =
      if i + n > h then false
      else if String.sub haystack i n = needle then true
      else loop (i + 1)
    in
    loop 0
  in
  let entries =
    match filter with
    | None -> entries_all
    | Some f when f = "" || f = "*" -> entries_all
    | Some f ->
        List.filter (fun (e:entry) -> contains ~needle:f ~haystack:(wire_name e)) entries_all
  in
  let entries =
    match limit with
    | None -> entries
    | Some n ->
        let rec take k xs =
          match k, xs with
          | k, _ when k <= 0 -> []
          | _, [] -> []
          | k, x::tl -> x :: take (k-1) tl
        in take n entries
  in
  let max_name  =
    List.fold_left (fun m (e:entry) -> max m (String.length (wire_name e))) 7 entries in
  let max_users =
    List.fold_left (fun m (e:entry) -> max m (String.length (string_of_int e.num_users))) 5 entries in
  let header = Printf.sprintf "%-*s  %*s  %s" max_name "Channel" max_users "Users" "Topic" in
  let sep = String.make (max_name + 2 + max_users + 2 + 5) '-' in
  let line_of (e:entry) =
    let topic = match e.topic with Some s -> s | None -> "" in
    Printf.sprintf "%-*s  %*d  %s"
      max_name (wire_name e) max_users e.num_users topic
  in
  let send s = notify t s in
  send header >>= fun () ->
  send sep    >>= fun () ->
  Lwt_list.iter_s (fun e -> send (line_of e)) entries

let user_find t nick =
  SMap.find_opt (user_key_of_nick nick) t.users

let user_ensure t nick =
  let k = user_key_of_nick nick in
  match SMap.find_opt k t.users with
  | Some u ->
      if u.nick <> nick then u.nick <- nick;
      u
  | None ->
      let u = User.make nick in
      t.users <- SMap.add k u t.users;
      u

let strip_mirc_codes (s : string) =
  let len = String.length s in
  let b = Bytes.create len in
  let is_digit i =
    i < len && let c = s.[i] in c >= '0' && c <= '9'
  in
  let rec eat_digits i n =
    if n = 0 || not (is_digit i) then i else eat_digits (i+1) (n-1)
  in
  let skip_color_params i =
    let i = eat_digits i 2 in
    if i < len && s.[i] = ',' then eat_digits (i+1) 2 else i
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

(* Ensure valid UTF-8; replace bad sequences with U+FFFD *)
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
    else if c < 0xC2 then (add_repl (); go (i+1))
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

(* Strip raw control chars (except \n, \r, \t), then remove mIRC codes, then fix UTF-8 *)
let sanitize_for_json (s : string) : string =
  let s =
    String.map
      (fun c ->
        if Char.code c < 0x20 && c <> '\n' && c <> '\r' && c <> '\t'
        then ' '
        else c)
      s
  in
  ensure_valid_utf8 (strip_mirc_codes s)

let opt_map f = function None -> None | Some x -> Some (f x)

let json_of_user (u : User.t) : Yojson.Safe.t =
  let s  = sanitize_for_json in
  let so = opt_map s in
  let who =
    match u.whois with
    | None -> `Null
    | Some w ->
        `Assoc [
          ("user",        (match so w.user        with Some x -> `String x | None -> `Null));
          ("host",        (match so w.host        with Some x -> `String x | None -> `Null));
          ("realname",    (match so w.realname    with Some x -> `String x | None -> `Null));
          ("server",      (match so w.server      with Some x -> `String x | None -> `Null));
          ("server_info", (match so w.server_info with Some x -> `String x | None -> `Null));
          ("account",     (match so w.account     with Some x -> `String x | None -> `Null));
          ("channels",    `List (List.map (fun c -> `String (s c)) w.channels));
          ("idle_secs",   (match w.idle_secs with Some x -> `Int x | None -> `Null));
          ("signon_ts",   (match w.signon_ts with Some x -> `Float x | None -> `Null));
          ("actual_host", (match so w.actual_host with Some x -> `String x | None -> `Null));
          ("secure",      (match w.secure with Some x -> `Bool x | None -> `Null));
        ]
  in
  let channel_modes_json =
    let pairs =
      User.StringMap.bindings u.channel_modes
      |> List.map (fun (k, ms) ->
            (Model_channel.wire_of_name k,
              `List (List.map (fun m -> `String (s m)) ms)))
    in
    `Assoc pairs
  in
  `Assoc [
    ("nick",      `String (s u.nick));
    ("real_name", (match so u.real_name with Some x -> `String x | None -> `Null));
    ("ident",     (match so u.ident     with Some x -> `String x | None -> `Null));
    ("host",      (match so u.host      with Some x -> `String x | None -> `Null));
    ("account",   (match so u.account   with Some x -> `String x | None -> `Null));
    ("away",      (match u.away with Some x -> `Bool x | None -> `Null));
    ("whois",     who);
    ("modes",     `List (List.map (fun m -> `String (s m)) u.modes));
    ("channel_modes", channel_modes_json);
  ]

let emit_console_user_upsert (t : t) ~(u : User.t) : unit Lwt.t =
  let payload =
    `Assoc [
      ("type", `String "user");
      ("op",   `String "upsert");
      ("ts",   `Float (Unix.gettimeofday ()));
      ("user", json_of_user u)
    ]
  in
  (* Intentionally using CLIENT prefix per requirement *)
  notify t ("CLIENT " ^ Yojson.Safe.to_string payload)

let upsert_realname (t : t) ~(nick:string) ~(realname:string) : unit Lwt.t =
  let u = user_ensure t nick in
  if u.real_name <> Some realname then u.real_name <- Some realname;
  emit_console_user_upsert t ~u

(* time-gated WHOIS request *)
let whois_refresh_period = 180.0   (* 3 minutes *)

let whois_request (t : t) (nick : string) : unit Lwt.t =
  let key = user_key_of_nick nick in
  let now = Unix.gettimeofday () in
  let fresh =
    match SMap.find_opt key t.whois_last_ts with
    | None -> false
    | Some ts -> (now -. ts) < whois_refresh_period
  in
  if fresh then (
    (* Serve from cache if we have the user; emit the same JSON we use on 318 *)
    match SMap.find_opt (user_key_of_nick nick) t.users with
    | None ->
        notify t (Printf.sprintf "WHOIS cache miss for %s; querying server…" nick) >>= fun () ->
        send_raw t (Printf.sprintf "WHOIS %s\r\n" nick)
    | Some u ->
        emit_console_user_upsert t ~u
  ) else (
    t.whois_last_ts <- SMap.add key now t.whois_last_ts;
    send_raw t (Printf.sprintf "WHOIS %s\r\n" nick)
  )

(* existing, time-gated LIST with settle delay (kept as-is) *)
let list_refresh_period = 180.0   (* 3 minutes *)
let list_settle_seconds = 2.0

let get_channels t : Channel_list.t Lwt.t =
  let now = Unix.gettimeofday () in
  let allow_refresh =
    match t.last_list_refresh with
    | None -> true
    | Some ts -> (now -. ts) >= list_refresh_period
  in
  if not allow_refresh then
    Lwt.return t.chanlist
  else begin
    t.last_list_refresh <- Some now;
    send_raw t "LIST\r\n" >>= fun () ->
    Lwt_unix.sleep list_settle_seconds >>= fun () ->
    Lwt.return t.chanlist
  end

let get_channels_unit t =
  get_channels t >>= fun _ ->
  Lwt.return_unit

let json_of_chanlist ?filter ?limit (t : t) : Yojson.Safe.t =
  let s = sanitize_for_json in
  let open Channel_list in
  let entries =
    Map.bindings t.chanlist
    |> List.map (fun (_k, e) ->
          `Assoc [
            ("name",      `String (s (wire_of_name e.name)));
            ("num_users", `Int e.num_users);
            ("topic",     match e.topic with Some x -> `String (s x) | None -> `Null);
          ])
  in
  `Assoc [
    ("type",    `String "chanlist");
    ("ts",      `Float (Unix.gettimeofday ()));
    ("entries", `List entries);
    ("filter",  match filter with Some x -> `String (s x) | None -> `Null);
    ("limit",   match limit  with Some n -> `Int n        | None -> `Null);
  ]

let emit_client_blob t (j : Yojson.Safe.t) : unit Lwt.t =
  let line = "CLIENT " ^ (Yojson.Safe.to_string j) in
  notify t line

(* Emit a dedicated nick change event the UI can consume for toasts/annotations. *)
let emit_client_nick_change
    (t : t)
    ~(old_nick:string)
    ~(new_nick:string)
    ~(channels:string list)
  : unit Lwt.t =
  let key_old = user_key_of_nick old_nick in
  let key_new = user_key_of_nick new_nick in
  let payload =
    `Assoc [
      ("type",      `String "nick_change");
      ("ts",        `Float (Unix.gettimeofday ()));
      ("old_nick",  `String (sanitize_for_json old_nick));
      ("new_nick",  `String (sanitize_for_json new_nick));
      ("old_key",   `String key_old);
      ("new_key",   `String key_new);
      ("channels",  `List (List.map (fun c -> `String (sanitize_for_json c)) channels));
    ]
  in
  emit_client_blob t payload


(* --- Singular “self user” helpers --- *)
let emit_client_user_pointer (t : t) ~(nick:string) : unit Lwt.t =
  let key = user_key_of_nick nick in
  let payload =
    `Assoc [
      ("type", `String "client_user");
      ("op",   `String "pointer");
      ("ts",   `Float (Unix.gettimeofday ()));
      ("key",  `String key);
      ("nick", `String nick);
    ]
  in
  emit_client_blob t payload

let set_self_by_nick (t : t) (nick : string) : unit Lwt.t =
  let u = user_ensure t nick in
  t.self_user <- Some u;
  emit_client_user_pointer t ~nick

let sync_self_user (t : t) : unit Lwt.t =
  match t.self_user with
  | None -> Lwt.return_unit
  | Some u ->
      let key = user_key_of_nick u.nick in
      let payload =
        `Assoc [
          ("type", `String "client_user");
          ("op",   `String "upsert");
          ("ts",   `Float (Unix.gettimeofday ()));
          ("key",  `String key);
          ("user", json_of_user u);
        ]
      in
      emit_client_blob t payload

(* Channels → UI sync (snapshot / upsert / remove) *)
let set_to_list (s : Channel.StringSet.t) : string list =
  Channel.StringSet.elements s

let json_of_channel (ch : Channel.t) : Yojson.Safe.t =
  let s = sanitize_for_json in
  let max_emit = 3000 in (* Max Records it can churn *)
  let take_with_more n xs =
    let rec take k acc = function
      | [] -> (List.rev acc, 0)
      | _ when k = 0 -> (List.rev acc, List.length xs - List.length acc)
      | x::tl -> take (k-1) (x::acc) tl
    in
    let (front, more) = take n [] xs in
    if more > 0 then front @ [Printf.sprintf "… (%d more)" more] else front
  in
  let list_json xs = `List (List.map (fun u -> `String (s u)) xs) in
  let users  = take_with_more max_emit (set_to_list ch.users) in
  let ops    = take_with_more max_emit (set_to_list ch.ops) in
  let voices = take_with_more max_emit (set_to_list ch.voices) in
  `Assoc [
    ("name",   `String (s (Channel.to_wire ch)));
    ("key",    `String (s (Channel.key_of_name ch.name)));
    ("topic",  (match ch.topic with Some t -> `String (s t) | None -> `Null));
    ("users",  list_json users);
    ("ops",    list_json ops);
    ("voices", list_json voices);
    ("modes",  `List (List.map (fun m -> `String (s m)) ch.modes));
  ]

let assoc_of_channels ?only (m : Channel.t SMap.t) : Yojson.Safe.t =
  let want =
    match only with
    | None -> SMap.bindings m
    | Some names ->
        let keys =
          List.fold_left (fun acc n -> SMap.add (channel_key_of n) () acc) SMap.empty names
        in
        SMap.bindings m |> List.filter (fun (k, _) -> SMap.mem k keys)
  in
  let pairs =
    List.map
      (fun (_k, ch) ->
        let wire = Channel.to_wire ch in
        (wire, json_of_channel ch))
      want
  in
  `Assoc pairs

let emit_channels_snapshot (t : t) : unit Lwt.t =
  let payload =
    `Assoc [
      ("type",      `String "channels");
      ("op",        `String "snapshot");
      ("channels",  assoc_of_channels t.channels);
      ("ts",        `Float (Unix.gettimeofday ()));
    ]
  in
  emit_client_blob t payload

let emit_channels_upsert (t : t) ~names : unit Lwt.t =
  let payload =
    `Assoc [
      ("type",      `String "channels");
      ("op",        `String "upsert");
      ("channels",  assoc_of_channels ~only:names t.channels);
      ("ts",        `Float (Unix.gettimeofday ()));
    ]
  in
  emit_client_blob t payload

let emit_channels_remove (t : t) ~names : unit Lwt.t =
  let wire_names = List.map Channel.wire_of_name names in
  let payload =
    `Assoc [
      ("type",   `String "channels");
      ("op",     `String "remove");
      ("names",  `List (List.map (fun s -> `String s) wire_names));
      ("ts",     `Float (Unix.gettimeofday ()));
    ]
  in
  emit_client_blob t payload

let join t ch =
  let _ = channel_ensure t ch in
  emit_channels_upsert t ~names:[ch] >>= fun () ->
  send_raw t (Printf.sprintf "JOIN %s\r\n" (Channel.wire_of_name ch))

(* Emit a single-channel blob for UI consumption *)
let emit_channel_info (t : t) ~name : unit Lwt.t =
  match channel_find t name with
  | None ->
      notify t
        (Printf.sprintf "You are not in %s" (Channel.wire_of_name name))
  | Some ch ->
      let payload =
        `Assoc [
          ("type",    `String "channel");
          ("ts",      `Float (Unix.gettimeofday ()));
          ("channel", json_of_channel ch);
        ]
      in
      emit_client_blob t payload

(* New flow: /list becomes “request”, synchronous dump if cached, otherwise LIST and dump on 323 *)
let list_request (t : t) ?filter ?limit () : unit Lwt.t =
  let now = Unix.gettimeofday () in
  let allow_refresh =
    match t.last_list_refresh with
    | None -> true
    | Some ts -> (now -. ts) >= list_refresh_period
  in
  t.last_list_args <- Some (filter, limit);
  if not allow_refresh then
    emit_client_blob t (json_of_chanlist ?filter ?limit t)
  else begin
    (* Hard refresh: clear, set gate, send LIST (optionally with pattern) *)
    t.chanlist <- Channel_list.empty;
    t.last_list_refresh <- Some now;
    let line =
      match filter with
      | None | Some "" | Some "*" -> "LIST\r\n"
      | Some pat -> Printf.sprintf "LIST %s\r\n" pat
    in
    send_raw t line
  end

module Default_cmd = Cmd_core.Make(struct
  type t       = client_ctx
  let send_raw = send_raw
  let join     = join
  let notify   = notify
  let list_request = list_request
  let channel_show (c:client_ctx) (ch:string) =
    emit_channel_info c ~name:ch
  let whois_request = whois_request
  let sync_self_user = sync_self_user
end)

let default_cmd () : cmd_pack =
  Pack ((module Default_cmd.Cmd), Default_cmd.Cmd.create ())

let cmd t ~key ~args =
  let (Pack ((module M), inst)) = t.cmd_pack in
  M.dispatch inst t ~key ~args

let cmd_async t ~key ~args =
  let (Pack ((module M), inst)) = t.cmd_pack in
  M.dispatch_async inst t ~key ~args

let create (c_mod : (module CONN)) (cfg : Conn.cfg)
            ~engine ~ui:(ui_mod : (module UIX.S)) ~opts ?cmd () =
  {
    cfg; engine; ui_mod; c_mod; c = None;
    to_client_m = Lwt_mvar.create_empty ();
    from_client_m = Lwt_mvar.create_empty ();
    acc = Engine.Acc.create ();
    opts; running = false;
    cmd_pack = (match cmd with Some p -> p | None -> default_cmd ());
    channels = SMap.empty;
    chanlist = Channel_list.empty;
    users = SMap.empty;
    self_user = None;
    last_list_refresh = None;
    last_list_args = None;
    names_refreshing = CSet.empty;
    whois_last_ts = SMap.empty;
    on_connect = ref [];
    registered = false;
    cap_active = false;
    sasl_in_progress = false;
    nick_cycle =
      (match opts.nick with Some n when String.trim n <> "" -> [n] | _ -> ["guest"])
      @ (List.filter (fun s -> String.trim s <> "") opts.alt_nicks);
    reg_timer = None;
    was_connected = false;
  }
  |> fun t ->
    (* ---------------- Handshake helpers ---------------- *)
    let choose = function Some s when String.trim s <> "" -> Some (String.trim s) | _ -> None in
    let current_nick () =
      match t.nick_cycle with
      | n :: _ -> n
      | []     -> "guest"
    in
    let rotate_nick () =
      match t.nick_cycle with
      | [] -> ()
      | [_] ->
          (* last resort: suffix '_' *)
          let n = current_nick () in
          t.nick_cycle <- [n ^ "_"]
      | _ :: rest -> t.nick_cycle <- rest
    in
    let username_for_user () =
      match choose t.cfg.username with
      | Some u -> u
      | None   -> current_nick ()
    in
    let realname_for_user () =
      match choose t.opts.realname with
      | Some rn -> rn
      | None ->
        (match choose t.cfg.realname with Some rn -> rn | None -> current_nick ())
    in
    (* Tiny base64 encoder for SASL PLAIN *)
    let b64_encode (s:string) : string =
      let tbl = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
      let len = String.length s in
      let buf = Buffer.create ((len + 2) / 3 * 4) in
      let byte i = Char.code s.[i] in
      let rec loop i =
        if i >= len then ()
        else
          let b0 = byte i in
          let b1 = if i+1 < len then byte (i+1) else 0 in
          let b2 = if i+2 < len then byte (i+2) else 0 in
          let n = (b0 lsl 16) lor (b1 lsl 8) lor b2 in
          Buffer.add_char buf tbl.[(n lsr 18) land 63];
          Buffer.add_char buf tbl.[(n lsr 12) land 63];
          if i+1 < len then Buffer.add_char buf tbl.[(n lsr 6) land 63] else Buffer.add_char buf '=';
          if i+2 < len then Buffer.add_char buf tbl.[n land 63]         else Buffer.add_char buf '=';
          loop (i+3)
      in loop 0; Buffer.contents buf
    in
    let send_pass_if_any () =
      match choose t.cfg.password with
      | Some pass -> send_raw t (Printf.sprintf "PASS %s\r\n" pass)
      | None      -> Lwt.return_unit
    in
    let send_nick_user () =
      let n  = current_nick () in
      let un = username_for_user () in
      let rn = realname_for_user () in
      set_self_by_nick t n >>= fun () ->
      upsert_realname t ~nick:n ~realname:rn >>= fun () ->
      sync_self_user t >>= fun () ->
      send_raw t (Printf.sprintf "NICK %s\r\n" n) >>= fun () ->
      send_raw t (Printf.sprintf "USER %s 0 * :%s\r\n" un rn)
    in
    let cap_begin_if_needed () =
      match t.opts.sasl_plain with
      | None -> Lwt.return_unit
      | Some _ ->
          t.cap_active <- true;
          send_raw t "CAP LS 302\r\n"
    in
    let cap_end () =
      if t.cap_active then (t.cap_active <- false; send_raw t "CAP END\r\n")
      else Lwt.return_unit
    in
    let start_sasl () =
      match t.opts.sasl_plain with
      | None -> Lwt.return_unit
      | Some _ ->
          t.sasl_in_progress <- true;
          send_raw t "CAP REQ :sasl\r\n"
    in
    let abort_if_required_sasl () =
      if t.opts.require_sasl then
        notify t "SASL required but failed; disconnecting." >>= fun () ->
        quit t
      else
        notify t "SASL failed; continuing without it." >>= fun () ->
        cap_end ()
    in
    let arm_registration_timer () =
      (match t.reg_timer with
      | Some _ -> ()
      | None ->
          let p =
            Lwt_unix.sleep t.opts.reg_timeout_s >>= fun () ->
            if not t.registered then
              let msg =
                if t.sasl_in_progress
                then "Still negotiating SASL…"
                else Printf.sprintf "Registration taking longer than %.0fs…" t.opts.reg_timeout_s
              in
              notify t msg >>= fun () ->
              (if t.cap_active then cap_end () else send_raw t (Printf.sprintf "NICK %s\r\n" (current_nick ())))
            else Lwt.return_unit
          in
          t.reg_timer <- Some p);
      Lwt.return_unit
    in

    (* Engine event bindings *)
    Engine.on t.engine "CONNECT"
      (fun _ _ ->
        t.registered <- false;
        send_pass_if_any () >>= fun () ->
        cap_begin_if_needed () >>= fun () ->
        send_nick_user ()     >>= fun () ->
        arm_registration_timer ()
      );

    (* CAP replies *)
    Engine.on t.engine "CAP"
      (fun ev _ ->
        match P.payload ev with
        | P.Other ("CAP", params, trailing) ->
            let p2 = (match params with _srv :: x :: _ -> String.uppercase_ascii x | x::[] -> String.uppercase_ascii x | _ -> "") in
            (match p2 with
              | "LS" ->
                  let caps = match trailing with Some s -> " " ^ String.lowercase_ascii s ^ " " | None -> " " in
                  (match t.opts.sasl_plain with
                  | Some _ when String.contains caps 's' && String.contains caps 'a' ->
                      (* naive contains; ok for " sasl " *)
                      if String.exists (fun _ -> false) caps then Lwt.return_unit else start_sasl ()
                  | Some _ ->
                      (* no sasl support *)
                      abort_if_required_sasl ()
                  | None -> cap_end ())
              | "ACK" ->
                (* Server acknowledged sasl *)
                (match t.opts.sasl_plain with
                  | Some _ ->
                      send_raw t "AUTHENTICATE PLAIN\r\n"
                  | None -> Lwt.return_unit)
              | "NAK" ->
                  abort_if_required_sasl ()
              | _ -> Lwt.return_unit)
        | _ -> Lwt.return_unit);

    (* AUTHENTICATE flow *)
    Engine.on t.engine "AUTHENTICATE"
      (fun ev _ ->
        match P.payload ev with
        | P.Other ("AUTHENTICATE", _ps, Some "+") ->
            (match t.opts.sasl_plain with
              | Some (user, pass) ->
                  let payload = "\x00" ^ user ^ "\x00" ^ pass in
                  let b = b64_encode payload in
                  send_raw t (Printf.sprintf "AUTHENTICATE %s\r\n" b)
              | None -> Lwt.return_unit)
        | _ -> Lwt.return_unit);

    (* SASL success / failure numerics *)
    Engine.on t.engine "903" (fun _ _ -> t.sasl_in_progress <- false; cap_end ());
    Engine.on t.engine "900" (fun _ _ -> t.sasl_in_progress <- false; Lwt.return_unit);
    Engine.on t.engine "904" (fun _ _ -> t.sasl_in_progress <- false; abort_if_required_sasl ());
    Engine.on t.engine "905" (fun _ _ -> t.sasl_in_progress <- false; abort_if_required_sasl ());
    Engine.on t.engine "906" (fun _ _ -> t.sasl_in_progress <- false; abort_if_required_sasl ());
    Engine.on t.engine "907" (fun _ _ -> t.sasl_in_progress <- false; abort_if_required_sasl ());

    (* 001 welcome -> we are registered *)
    Engine.on t.engine "001"
      (fun _ _ ->
        t.registered <- true;
        t.was_connected <- true;
        notify t "Registered (001)" );

    (* 433 nick in use -> rotate and try again *)
    Engine.on t.engine "433"
      (fun _ _ ->
        let old = current_nick () in
        rotate_nick ();
        let nn = current_nick () in
        notify t (Printf.sprintf "Nick %S is in use; trying %S…" old nn) >>= fun () ->
        send_raw t (Printf.sprintf "NICK %s\r\n" nn)
      );
    t

let start_ui t =
  let module UIM = (val t.ui_mod : UIX.S) in
  let ui = UIM.create () in
  let from_client () = Lwt_mvar.take t.from_client_m in
  let to_client msg  = Lwt_mvar.put t.to_client_m msg in
  UIM.run ui ~from_client ~to_client >>= fun () -> UIM.close ui

let start_net t =
  let module C = (val t.c_mod : CONN) in

  (* Tell the UI the client side is up, then explicitly WAIT for an
     inbound “CONNECT” command from the UI before dialing the IRC server. *)
  Lwt_mvar.put t.from_client_m UIX.UiConnected >>= fun () ->
  notify t "(Issue /connect ...)" >>= fun () ->

  (* Pre-connect loop: only “CONNECT” lets us proceed. Everything else is
     gently rejected until we’re connected. *)
  let rec wait_for_connect () =
    Lwt_mvar.take t.to_client_m >>= function
    | UIX.UiCmd (key_str, _args)
      when String.uppercase_ascii key_str = "CONNECT" ->
        Lwt.return_unit
    | UIX.UiQuit ->
        (* Abort net start cleanly *)
        Lwt.fail Exit
    | UIX.UiSendRaw _ ->
        notify t "Not connected yet. Type /connect to begin." >>= wait_for_connect
    | UIX.UiCmd (_k, _args) ->
        notify t "Not connected yet. Type /connect to begin." >>= wait_for_connect
  in

  Lwt.catch
    (fun () ->
      wait_for_connect () >>= fun () ->

      (* Now dial the IRC server. *)
      C.connect t.cfg >>= fun conn ->
      t.c <- Some (B ((module C), conn));

      (* Tell the engine we are connected; CONNECT handlers will identify. *)
      Engine.ingest_line t.engine "CONNECT" t >>= fun () ->

      (* Tell the UI what we know right now (may be empty). *)
      emit_channels_snapshot t >>= fun () ->

      let rec rx_loop () =
        let buf = Bytes.create 4096 in
        C.recv conn buf >>= function
        | 0 -> Lwt_mvar.put t.from_client_m UIX.ClientClosed
        | n ->
            let chunk = Bytes.sub_string buf 0 n in
            Lwt_mvar.put t.from_client_m (UIX.ClientRxChunk (Bytes.of_string chunk)) >>= fun () ->
            Engine.ingest_chunk t.engine t.acc chunk t >>= rx_loop
      in
      let rec tx_loop () =
        Lwt_mvar.take t.to_client_m >>= function
        | UIX.UiSendRaw b ->
            C.send conn ~off:0 ~len:(Bytes.length b) b >>= fun _ -> tx_loop ()
        | UIX.UiCmd (key_str, args) ->
            let key = Cmd_key.of_string key_str in
            cmd_async t ~key ~args; tx_loop ()
        | UIX.UiQuit ->
            Lwt.return_unit
      in
      Lwt.finalize (fun () -> Lwt.pick [ rx_loop (); tx_loop () ])
                    (fun () -> Lwt.catch (fun () -> C.close conn) (fun _ -> Lwt.return_unit))
    )
    (function
      | Exit -> Lwt.return_unit  (* user quit before connecting *)
      | _exn -> Lwt.return_unit)


let start t =
  if t.running then Lwt.return_unit
  else begin
    t.running <- true;

    let ui_task  = start_ui t in
    let net_task = start_net t in

    let on_ui_exit =
      ui_task >>= fun () ->
      (* UI adapter terminated: make sure the net side stops too. *)
      let signal_tx_loop_quit () =
        (* Best-effort, don’t block if something is already queued. *)
        if Lwt_mvar.is_empty t.to_client_m
        then Lwt_mvar.put t.to_client_m UIX.UiQuit
        else Lwt.return_unit
      in
      signal_tx_loop_quit () >>= fun () ->
      (if t.was_connected
        then Lwt.catch (fun () -> quit t) (fun _ -> Lwt.return_unit)
        else Lwt.return_unit) >>= fun () ->
      (* Close underlying connection if any; this unblocks rx_loop. *)
      (match t.c with
        | None -> Lwt.return_unit
        | Some (B ((module C), conn)) ->
            Lwt.catch (fun () -> C.close conn) (fun _ -> Lwt.return_unit))
    in

    (* Wait for UI to end and for net to wind down, then exit(0). *)
    Lwt.join [ on_ui_exit; net_task ] >|= fun () ->
    Stdlib.exit 0
  end

let stop t =
  t.running <- false;
  Lwt_mvar.put t.to_client_m UIX.UiQuit >>= fun () ->
  match t.c with
  | None -> Lwt.return_unit
  | Some (B ((module C), conn)) ->
      Lwt.catch (fun () -> C.close conn) (fun _ -> Lwt.return_unit)

(*  ChannelList helpers passthroughs *)
let channel_list_find t name =
  Channel_list.find name t.chanlist

let channel_list_upsert t ~name ~num_users ~topic =
  t.chanlist <- Channel_list.upsert ~name ~num_users ~topic t.chanlist

let chanlist_upsert_unit t ~name ~num_users ~topic =
  channel_list_upsert t ~name ~num_users ~topic;
  Lwt.return_unit

(* For compatibility / non-323 contexts *)
let get_and_emit_channels t ?filter ?limit () : unit Lwt.t =
  get_channels t >>= fun _snapshot ->
  emit_channel_list_dump ?filter ?limit t

let users_size t = SMap.cardinal t.users

let evict_user_by_key (t : t) (key : string) : unit Lwt.t =
  t.channels <- SMap.map (fun ch -> Channel.remove_all ch key) t.channels;
  t.users <- SMap.remove key t.users;
  Lwt.return_unit

let evict_user_sync (t : t) (nick : string) : unit =
  let key = User.key_of_nick nick in
  t.channels <- SMap.map (fun ch -> Channel.remove_all ch key) t.channels;
  t.users <- SMap.remove key t.users

let prune_orphan_members (t : t) : unit =
  let module SS = Channel.StringSet in
  let keep k = SMap.mem k t.users in
  let filter_set s =
    SS.fold (fun k acc -> if keep k then SS.add k acc else acc) s SS.empty
  in
  t.channels <-
    SMap.map
      (fun (ch : Channel.t) ->
        { ch with
          users  = filter_set ch.users;
          ops    = filter_set ch.ops;
          voices = filter_set ch.voices })
      t.channels

let evict_user (t : t) (nick : string) : unit Lwt.t =
  evict_user_sync t nick;
  Lwt.return_unit

(* Replace the table dump on LISTEND with the blob *)
let list_completed (t : t) : unit Lwt.t =
  let filter, limit =
    match t.last_list_args with
    | Some (f, l) -> (f, l)
    | None -> (None, None)
  in
  emit_client_blob t (json_of_chanlist ?filter ?limit t)

  (* NAMES (353/366) helpers *)

let names_prepare (t : t) (channel : string) : unit Lwt.t =
  let key = channel_key_of channel in
  if not (CSet.mem key t.names_refreshing) then begin
    let ch = channel_ensure t channel in
    let cleared =
      { ch with
        users  = Channel.StringSet.empty;
        ops    = Channel.StringSet.empty;
        voices = Channel.StringSet.empty; }
    in
    t.channels <- SMap.add key cleared t.channels;
    t.names_refreshing <- CSet.add key t.names_refreshing;
    (* Clear per-user channel modes for this channel so NAMES can rebuild them *)
    t.users <-
      SMap.map
        (fun (u:User.t) ->
            let cm =
              if User.StringMap.mem key u.channel_modes
              then User.StringMap.remove key u.channel_modes
              else u.channel_modes
            in
            { u with channel_modes = cm })
        t.users
  end;
  Lwt.return_unit

let names_member (t : t) ~ch ~nick ~status : unit Lwt.t =
  ignore (user_ensure t nick);  (* upsert authoritative user object *)
  let user_key = user_key_of_nick nick in
  let kch = channel_key_of ch in
  let ch_obj = channel_ensure t ch in
  let ch1 = Channel.add_user ch_obj user_key in
  let ch2 =
    match status with
    | `Op    -> Channel.add_op ch1 user_key
    | `Voice -> Channel.add_voice ch1 user_key
    | `User  -> ch1
  in
  t.channels <- SMap.add kch ch2 t.channels;
  (* Update user's per-channel modes (e.g., @ -> "o", + -> "v") *)
  let u = user_ensure t nick in
  let open User in
  let add uniq xs = if List.exists ((=) uniq) xs then xs else xs @ [uniq] in
  let mode_opt =
    match status with
    | `Op    -> Some Mode.operator_mode
    | `Voice -> Some Mode.voice_mode
    | `User  -> None
  in
  (match mode_opt with
    | None -> ()
    | Some m ->
      let prev = match StringMap.find_opt kch u.channel_modes with None -> [] | Some xs -> xs in
      let next = add m prev in
      u.channel_modes <- StringMap.add kch next u.channel_modes);
  Lwt.return_unit

let names_completed (t : t) (channel : string) : unit Lwt.t =
  let key = channel_key_of channel in
  t.names_refreshing <- CSet.remove key t.names_refreshing;
  emit_channels_upsert t ~names:[channel]

(* end NAMES helpers *)

let member_join (t : t) ~ch ~nick : unit Lwt.t =
  ignore (user_ensure t nick);
  let user_key = user_key_of_nick nick in
  let kch = channel_key_of ch in
  let ch_obj = channel_ensure t ch in
  let ch' = Channel.add_user ch_obj user_key in
  t.channels <- SMap.add kch ch' t.channels;
  (* fast UI hint *)
  emit_channels_upsert t ~names:[ch]

let member_part (t : t) ~ch ~nick ~reason:_ : unit Lwt.t =
  let user_key = user_key_of_nick nick in
  let kch = channel_key_of ch in
  match SMap.find_opt kch t.channels with
  | None -> Lwt.return_unit
  | Some ch_obj ->
      let ch' = Channel.remove_all ch_obj user_key in
      t.channels <- SMap.add kch ch' t.channels;
      (match user_find t nick with
        | None -> ()
        | Some u ->
          u.channel_modes <- User.StringMap.remove kch u.channel_modes);
      emit_channels_upsert t ~names:[ch]

(* Set/replace a channel's topic and upsert to UI *)
let channel_set_topic (t : t) ~ch ~topic : unit Lwt.t =
  let kch   = channel_key_of ch in
  let chobj = channel_ensure t ch in
  let ch'   = Channel.set_topic chobj (Some topic) in
  t.channels <- SMap.add kch ch' t.channels;
  emit_channels_upsert t ~names:[ch]


(* ---------------- WHOIS helpers ---------------- *)
let ensure_whois (u : User.t) : User.whois =
  match u.whois with
  | Some w -> w
  | None -> {
      user=None; host=None; realname=None;
      server=None; server_info=None; account=None;
      channels=[]; idle_secs=None; signon_ts=None;
      actual_host=None; secure=None;
    }

let set_user t k u =
  t.users <- SMap.add k u t.users

let whois_basic (t : t) ~nick ~user ~host ~realname =
  let k = user_key_of_nick nick in
  let u = user_ensure t nick in
  let w = ensure_whois u in
  let w' = { w with user=Some user; host=Some host; realname } in
  u.whois <- Some w';
  set_user t k u;
  Lwt.return_unit

let whois_server (t : t) ~nick ~server ~server_info =
  let k = user_key_of_nick nick in
  let u = user_ensure t nick in
  let w = ensure_whois u in
  let w' = { w with server=Some server; server_info } in
  u.whois <- Some w';
  set_user t k u;
  Lwt.return_unit

let whois_channels (t : t) ~nick ~channels =
  let k = user_key_of_nick nick in
  let u = user_ensure t nick in
  let w = ensure_whois u in
  let w' = { w with channels } in
  u.whois <- Some w';
  set_user t k u;
  Lwt.return_unit

let whois_actual (t : t) ~nick ~actual_host =
  let k = user_key_of_nick nick in
  let u = user_ensure t nick in
  let w = ensure_whois u in
  let w' = { w with actual_host = Some actual_host } in
  u.whois <- Some w';
  set_user t k u;
  Lwt.return_unit

let whois_secure (t : t) ~nick =
  let k = user_key_of_nick nick in
  let u = user_ensure t nick in
  let w = ensure_whois u in
  let w' = { w with secure = Some true } in
  u.whois <- Some w';
  set_user t k u;
  Lwt.return_unit

let whois_complete (t : t) ~nick =
  let _k = user_key_of_nick nick in
  let u = user_ensure t nick in
  emit_console_user_upsert t ~u

(* MODE parsing helpers *)
let is_status_mode (m : char) =
  match m with
  | 'o' | 'v' | 'h' | 'q' | 'a' -> true
  | _ -> false

let mode_requires_arg_for_channel (sign:char) (m:char) =
  match m, sign with
  | ('o'|'v'|'h'|'q'|'a'|'k'|'l'|'b'|'e'|'I'), '+' -> true
  | ('o'|'v'|'h'|'q'|'a'), '-' -> true
  | _ -> false

(* NOTE: users_ref has the precise type (User.t SMap.t) ref — fixes the SMap error. *)
let apply_mode_string_channel
    ~(ch:Channel.t)
    ~(kch:string)
    ~(users_ref:User.t SMap.t ref)
    ~(modestr:string)
    ~(args:string list)
  : Channel.t * User.t list =
  let sign = ref '+' in
  let args = ref args in
  let changed_users = ref [] in
  let add_user_change u = changed_users := u :: !changed_users in

  let take_arg () =
    match !args with
    | a::tl -> args := tl; Some a
    | [] -> None
  in

  let add_ch_mode ch m = Channel.add_mode ch (String.make 1 m) in
  let rem_ch_mode ch m = Channel.remove_mode ch (String.make 1 m) in

  let add_user_status ch m nick =
    let uk = user_key_of_nick nick in
    let u =
      match SMap.find_opt uk !users_ref with
      | Some u -> u
      | None ->
          let u = User.make nick in
          users_ref := SMap.add uk u !users_ref; u
    in
    (* use provided channel key kch (avoid touching record fields) *)
    let prior = match User.StringMap.find_opt kch u.channel_modes with None -> [] | Some xs -> xs in
    let mcode =
      match m with
      | 'o' -> Some User.Mode.operator_mode
      | 'v' -> Some User.Mode.voice_mode
      | 'h' -> Some User.Mode.halfop_mode
      | 'q' -> Some User.Mode.owner_mode
      | 'a' -> Some User.Mode.admin_mode
      | _   -> None
    in
    (match mcode with
     | None -> ()
     | Some code ->
         if not (List.exists ((=) code) prior) then
           u.channel_modes <- User.StringMap.add kch (prior @ [code]) u.channel_modes);
    add_user_change u;
    let ch =
      match m with
      | 'o' | 'q' | 'a' | 'h' -> Channel.add_op ch uk
      | 'v'                   -> Channel.add_voice ch uk
      | _ -> ch
    in
    ch
  in

  let rem_user_status ch m nick =
    let uk = user_key_of_nick nick in
    (match SMap.find_opt uk !users_ref with
     | Some u ->
         (* use provided channel key kch *)
         let code_opt =
           match m with
           | 'o' -> Some User.Mode.operator_mode
           | 'v' -> Some User.Mode.voice_mode
           | 'h' -> Some User.Mode.halfop_mode
           | 'q' -> Some User.Mode.owner_mode
           | 'a' -> Some User.Mode.admin_mode
           | _   -> None
         in
         (match code_opt with
          | None -> ()
          | Some code ->
              let prior = match User.StringMap.find_opt kch u.channel_modes with None -> [] | Some xs -> xs in
              let next  = List.filter ((<>) code) prior in
              if List.length next <> List.length prior then
                u.channel_modes <-
                  (if next = [] then User.StringMap.remove kch u.channel_modes
                   else User.StringMap.add kch next u.channel_modes);
              add_user_change u)
     | None -> ());
    let ch =
      match m with
      | 'o' | 'q' | 'a' | 'h' -> Channel.remove_op ch uk
      | 'v'                   -> Channel.remove_voice ch uk
      | _ -> ch
    in
    ch
  in

  let rec loop i ch =
    if i >= String.length modestr then (ch, List.rev !changed_users)
    else
      let c = modestr.[i] in
      if c = '+' || c = '-' then (sign := c; loop (i+1) ch)
      else
        let ch' =
          if is_status_mode c && mode_requires_arg_for_channel !sign c then
            match take_arg () with
            | Some nick when !sign = '+' -> add_user_status ch c nick
            | Some nick (* '-' *)       -> rem_user_status ch c nick
            | None                      -> ch
          else
            if !sign = '+' then add_ch_mode ch c else rem_ch_mode ch c
        in
        loop (i+1) ch'
  in
  loop 0 ch

let channel_mode_change (t : t) ~ch ~mode ~args : unit Lwt.t =
  let kch = channel_key_of ch in
  let ch_obj = channel_ensure t ch in
  let users_ref = ref t.users in
  let (ch', _changed_users) =
    apply_mode_string_channel ~ch:ch_obj ~kch ~users_ref ~modestr:mode ~args
  in
  t.channels <- SMap.add kch ch' t.channels;
  t.users    <- !users_ref;
  emit_channels_upsert t ~names:[ch]

let user_mode_change (t : t) ~nick ~mode : unit Lwt.t =
  let u = user_ensure t nick in
  let add uniq xs = if List.exists ((=) uniq) xs then xs else xs @ [uniq] in
  let rec walk i sign modes =
    if i >= String.length mode then modes
    else
      let c = mode.[i] in
      if c = '+' || c = '-' then walk (i+1) c modes
      else
        let mstr = String.make 1 c in
        let modes' =
          if sign = '+'
          then add mstr modes
          else List.filter ((<>) mstr) modes
        in
        walk (i+1) sign modes'
  in
  let next = walk 0 '+' u.modes in
  if next <> u.modes then (
    u.modes <- next;
    emit_console_user_upsert t ~u
  ) else Lwt.return_unit


(* NICK change propagation *)
let user_nick_change (t : t) ~old_nick ~new_nick : unit Lwt.t =
  let oldk = user_key_of_nick old_nick in
  let newk = user_key_of_nick new_nick in
  if oldk = newk then Lwt.return_unit else
  let (u_opt, users') =
    match SMap.find_opt oldk t.users with
    | None -> (None, t.users)  (* unseen? nothing to migrate *)
    | Some u ->
        u.nick <- new_nick;
        (Some u, SMap.add newk u (SMap.remove oldk t.users))
  in
  t.users <- users';
  (* Rewrite membership sets in all channels *)
  let changed_names = ref [] in
  let rewrite_set s =
    if Channel.StringSet.mem oldk s then
      Some (Channel.StringSet.add newk (Channel.StringSet.remove oldk s))
    else None
  in
  t.channels <-
    SMap.mapi
      (fun kch (ch : Channel.t) ->
        let users'  = rewrite_set ch.users  in
        let ops'    = rewrite_set ch.ops    in
        let voices' = rewrite_set ch.voices in
        match (users', ops', voices') with
        | (None, None, None) -> ch
        | _ ->
            let ch' =
              { ch with
                users  = (match users'  with Some s -> s | None -> ch.users);
                ops    = (match ops'    with Some s -> s | None -> ch.ops);
                voices = (match voices' with Some s -> s | None -> ch.voices) }
            in
            changed_names := (Model_channel.wire_of_name kch) :: !changed_names;
            ch')
      t.channels;
  (* If this is the connected user, retarget the pointer and emit *)
  (match t.self_user with
    | Some su when user_key_of_nick su.nick = oldk ->
        su.nick <- new_nick;
        (* pointer then full upsert to keep UI fast/correct *)
        emit_client_user_pointer t ~nick:new_nick >>= fun () ->
        sync_self_user t
    | _ -> Lwt.return_unit) >>= fun () ->

  (* Emit a high-level UI event for toasts/inline markers *)
  let channels_changed = List.rev !changed_names in
  emit_client_nick_change
    t ~old_nick ~new_nick ~channels:channels_changed >>= fun () ->

  (* Update any channels that changed *)
  (match !changed_names with
    | [] -> Lwt.return_unit
    | names -> emit_channels_upsert t ~names) >>= fun () ->
  (* And emit a user upsert if we had the user *)
  (match u_opt with
    | None -> Lwt.return_unit
    | Some u -> emit_console_user_upsert t ~u)
