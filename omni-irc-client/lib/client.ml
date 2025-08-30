(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
open Lwt.Infix

module UIX    = Irc_ui.Ui_intf
module Conn   = Irc_conn.Connector
module Engine = Irc_engine.Engine
module Event  = Irc_engine.Event

module type CONN = sig
  type conn
  val connect : Conn.cfg -> conn Lwt.t
  val recv    : conn -> bytes -> int Lwt.t
  val send    : conn -> ?off:int -> ?len:int -> bytes -> int Lwt.t
  val close   : conn -> unit Lwt.t
end

type opts = {
  nick     : string option;
  realname : string option;
}

(* existential to hold any concrete connection + its ops *)
type boxed =
  | B : (module CONN with type conn = 'c) * 'c -> boxed

type t = {
  cfg        : Conn.cfg;
  engine     : t Engine.t;

  (* UI *)
  ui_mod     : (module UIX.S);
  (* remove: mutable ui *)

  (* NET *)
  c_mod      : (module CONN);
  mutable c  : boxed option;

  to_client_m   : UIX.to_client Lwt_mvar.t;
  from_client_m : UIX.from_client Lwt_mvar.t;

  rx_buf     : Buffer.t;
  opts       : opts;
  mutable running : bool;
}

let create (c_mod : (module CONN)) (cfg : Conn.cfg) ~engine
          ~ui:(ui_mod : (module UIX.S)) ~opts =
  { cfg; engine; ui_mod; c_mod; c = None;
    to_client_m = Lwt_mvar.create_empty ();
    from_client_m = Lwt_mvar.create_empty ();
    rx_buf = Buffer.create 8192; opts; running = false; }

let with_conn (t : t) (f : boxed -> 'r Lwt.t) : 'r Lwt.t =
  match t.c with
  | None -> Lwt.fail_with "client: not connected"
  | Some b -> f b

let send_raw t line =
  with_conn t (fun (B ((module C), conn)) ->
    let len = String.length line in
    C.send conn ~off:0 ~len (Bytes.of_string line) >>= fun _ ->
    Lwt.return_unit)

let join t ch = send_raw t (Printf.sprintf "JOIN %s\r\n" ch)
let notify t s = Lwt_mvar.put t.from_client_m (UIX.ClientInfo s)
let quit   t   = send_raw t "QUIT :bye\r\n"

let flush_lines t =
  let s = Buffer.contents t.rx_buf in
  let parts = String.split_on_char '\n' s in
  let rev = List.rev parts in
  let last = match rev with [] -> "" | hd :: _ -> hd in
  let complete = match rev with [] -> [] | _ :: tl -> List.rev tl in
  Buffer.clear t.rx_buf; Buffer.add_string t.rx_buf last;
  Lwt_list.iter_p
    (fun l ->
      let l =
        if String.length l > 0 && l.[String.length l - 1] = '\r'
        then String.sub l 0 (String.length l - 1) else l
      in
      Lwt_mvar.put t.from_client_m (UIX.ClientRxChunk (Bytes.of_string (l ^ "\n"))) >>= fun () ->
      let ev = Irc_engine.Event.of_irc_line l in
      Irc_engine.Engine.emit_async t.engine ev t;
      Lwt.return_unit)
    complete

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
    (* optional NICK/USER *)
    (match t.opts.nick with
      | Some n when n <> "" -> send_raw t (Printf.sprintf "NICK %s\r\n" n)
      | _ -> Lwt.return_unit) >>= fun () ->
    (match t.opts.realname with
      | Some rn ->
        let user = match t.opts.nick with Some n when n <> "" -> n | _ -> "guest" in
        send_raw t (Printf.sprintf "USER %s 0 * :%s\r\n" user rn)
      | None -> Lwt.return_unit) >>= fun () ->
    (* RX/TX loops *)
    let rec rx_loop () =
      let buf = Bytes.create 4096 in
      C.recv conn buf >>= function
      | 0 -> Lwt_mvar.put t.from_client_m UIX.ClientClosed
      | n ->
          Buffer.add_string t.rx_buf (Bytes.sub_string buf 0 n);
          flush_lines t >>= rx_loop
    in
    let rec tx_loop () =
      Lwt_mvar.take t.to_client_m >>= function
      | UIX.UiSendRaw b ->
          C.send conn ~off:0 ~len:(Bytes.length b) b >>= fun _ -> tx_loop ()
      | UIX.UiQuit ->
          Lwt.return_unit
    in
  Lwt.finalize
    (fun () -> Lwt.pick [ rx_loop (); tx_loop () ])
    (fun () -> Lwt.catch (fun () -> C.close conn) (fun _ -> Lwt.return_unit))

let start t =
  if t.running then Lwt.return_unit else
  (t.running <- true;
    Lwt.pick [ start_ui t; start_net t ])

let stop t =
  t.running <- false;
  (* signal UI to end; close NET in start_net finalizer *)
  Lwt_mvar.put t.to_client_m UIX.UiQuit >>= fun () ->
  match t.c with
  | None -> Lwt.return_unit
  | Some (B ((module C), conn)) ->
      Lwt.catch (fun () -> C.close conn) (fun _ -> Lwt.return_unit)
