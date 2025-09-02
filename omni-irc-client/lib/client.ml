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

type opts = { nick : string option; realname : string option }

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
  mutable channels  : Channel.t SMap.t;
  mutable chanlist  : Channel_list.t;
  mutable users     : User.t SMap.t;

  (* time-gating for LIST *)
  mutable last_list_refresh : float option;
  mutable last_list_args    : (string option * int option) option;
  mutable names_refreshing  : CSet.t;
}
and cmd_pack = Pack : (module CMD with type ctx = t and type t = 'a) * 'a -> cmd_pack

type client_ctx = t

(* re-export so Client.Cmd_key works *)
module Cmd_key = Cmd_key

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

let notify t s = Lwt_mvar.put t.from_client_m (UIX.ClientInfo s)
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

let json_of_chanlist ?filter ?limit (t : t) : Yojson.Safe.t =
  let open Channel_list in
  let entries =
    Map.bindings t.chanlist
    |> List.map (fun (_k, e) ->
          `Assoc [
            ("name",      `String (wire_of_name e.name));
            ("num_users", `Int e.num_users);
            ("topic",     match e.topic with Some s -> `String s | None -> `Null)
          ])
  in
  `Assoc [
    ("type",    `String "chanlist");
    ("ts",      `Float (Unix.gettimeofday ()));
    ("entries", `List entries);
    ("filter",  match filter with Some s -> `String s | None -> `Null);
    ("limit",   match limit  with Some n -> `Int n    | None -> `Null)
  ]

let emit_client_blob t (j : Yojson.Safe.t) : unit Lwt.t =
  let line = "CLIENT " ^ (Y.to_string j) in
  notify t line

(* -------- Channels → UI sync (snapshot / upsert / remove) -------- *)
let set_to_list (s : Channel.StringSet.t) : string list =
  Channel.StringSet.elements s

let json_of_channel (ch : Channel.t) : Yojson.Safe.t =
  `Assoc [
    ("name",   `String (Channel.to_wire ch));                 (* display/wire name (key) *)
    ("key",    `String (Channel.key_of_name ch.name));        (* canonical, sans '#' *)
    ("topic",  (match ch.topic with Some s -> `String s | None -> `Null));
    ("users",  `List (List.map (fun u -> `String u) (set_to_list ch.users)));
    ("ops",    `List (List.map (fun u -> `String u) (set_to_list ch.ops)));
    ("voices", `List (List.map (fun u -> `String u) (set_to_list ch.voices)));
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
    last_list_refresh = None;
    last_list_args = None;
    names_refreshing = CSet.empty;
  }

let start_ui t =
  let module UIM = (val t.ui_mod : UIX.S) in
  let ui = UIM.create () in
  let from_client () = Lwt_mvar.take t.from_client_m in
  let to_client msg  = Lwt_mvar.put t.to_client_m msg in
  UIM.run ui ~from_client ~to_client >>= fun () -> UIM.close ui

let start_net t =
  let module C = (val t.c_mod : CONN) in
  C.connect t.cfg >>= fun conn ->
  t.c <- Some (B ((module C), conn));
  (match t.opts.nick with
    | Some n when n <> "" -> send_raw t (Printf.sprintf "NICK %s\r\n" n)
    | _ -> Lwt.return_unit) >>= fun () ->
  (match t.opts.realname with
    | Some rn ->
        let user = match t.opts.nick with Some n when n <> "" -> n | _ -> "guest" in
        send_raw t (Printf.sprintf "USER %s 0 * :%s\r\n" user rn)
    | None -> Lwt.return_unit) >>= fun () ->

  (* tell the UI what we know right now (may be empty) *)
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

let start t =
  if t.running then Lwt.return_unit
  else (t.running <- true; Lwt.pick [ start_ui t; start_net t ])

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

(* For compatibility / non-323 contexts *)
let get_and_emit_channels t ?filter ?limit () : unit Lwt.t =
  get_channels t >>= fun _snapshot ->
  emit_channel_list_dump ?filter ?limit t

(* Users (authoritative) *)
let user_key_of_nick = User.key_of_nick

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
    t.names_refreshing <- CSet.add key t.names_refreshing
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
  (match SMap.find_opt kch t.channels with
    | None -> Lwt.return_unit
    | Some ch_obj ->
      let ch' = Channel.remove_all ch_obj user_key in
      t.channels <- SMap.add kch ch' t.channels;
      emit_channels_upsert t ~names:[ch])
