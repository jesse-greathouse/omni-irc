module type CLIENT = sig
  type t
  val send_raw : t -> string -> unit Lwt.t
  val join     : t -> string -> unit Lwt.t
  val notify   : t -> string -> unit Lwt.t
end

module Make (C : CLIENT) = struct
  module Cmd = struct
    type ctx = C.t
    type t = unit
    let create () = ()

    let ensure_channel s =
      if s = "" then s else
      match s.[0] with '#' | '&' -> s | _ -> "#" ^ s

    let do_join (c:ctx) = function
      | ch :: _ -> C.join c (ensure_channel ch)
      | []      -> C.notify c "JOIN requires a channel argument"

    let do_names (c:ctx) = function
      | []      -> C.send_raw c "NAMES\r\n"
      | ch :: _ -> C.send_raw c (Printf.sprintf "NAMES %s\r\n" (ensure_channel ch))

    let do_raw (c:ctx) (args:string list) =
      let line = String.concat " " args in
      let line =
        if String.length line >= 2
          && line.[String.length line - 2] = '\r'
          && line.[String.length line - 1] = '\n'
        then line else line ^ "\r\n"
      in
      C.send_raw c line

    let dispatch _t (c:ctx) ~key ~args =
      let f =
        match key with
        | Cmd_key.Join      -> do_join
        | Cmd_key.Names     -> do_names
        | Cmd_key.Raw       -> do_raw
        | Cmd_key.Custom _  -> (fun _ _ -> Lwt.return_unit)
      in
      Lwt.catch (fun () -> f c args) (fun _ -> Lwt.return_unit)

    let dispatch_async t c ~key ~args =
      Lwt.async (fun () -> dispatch t c ~key ~args)
  end
end
