(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
open Lwt.Infix

module P      = Irc_engine.Parser
module UIX    = Irc_ui.Ui_intf
module Conn   = Irc_conn.Connector
module Engine = Irc_engine.Engine.Make(P)

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

type t = {
  cfg           : Conn.cfg;
  engine        : t Engine.t;
  ui_mod        : (module UIX.S);
  c_mod         : (module CONN);
  mutable c     : boxed option;
  to_client_m   : UIX.to_client Lwt_mvar.t;
  from_client_m : UIX.from_client Lwt_mvar.t;
  acc           : Engine.Acc.t;
  opts          : opts;
  mutable running : bool;
  cmd_pack      : cmd_pack;
}
and cmd_pack = Pack : (module CMD with type ctx = t and type t = 'a) * 'a -> cmd_pack

type client_ctx = t

(* re-export in the implementation so Client.Cmd_key works *)
module Cmd_key = Cmd_key

let with_conn (t : t) (f : boxed -> 'r Lwt.t) : 'r Lwt.t =
  match t.c with None -> Lwt.fail_with "client: not connected" | Some b -> f b

let send_raw t line =
  with_conn t (fun (B ((module C), conn)) ->
    let len = String.length line in
    C.send conn ~off:0 ~len (Bytes.of_string line) >>= fun _ -> Lwt.return_unit)

let join t ch = send_raw t (Printf.sprintf "JOIN %s\r\n" ch)
let notify t s = Lwt_mvar.put t.from_client_m (UIX.ClientInfo s)
let quit   t   = send_raw t "QUIT :bye\r\n"

module Default_cmd = Cmd_core.Make(struct
  type t       = client_ctx
  let send_raw = send_raw
  let join     = join
  let notify   = notify
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
        (* convert boundary string to typed key; never block IO loop *)
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
