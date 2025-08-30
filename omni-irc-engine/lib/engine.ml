(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

type 'ctx handler = Event.t -> 'ctx -> unit Lwt.t
type 'ctx t = (string, 'ctx handler list) Hashtbl.t

let create () = Hashtbl.create 32

let on tbl name h =
  let prev = match Hashtbl.find_opt tbl name with Some hs -> hs | None -> [] in
  Hashtbl.replace tbl name (prev @ [h])

let emit tbl ev ctx =
  match Hashtbl.find_opt tbl (Event.name ev) with
  | None -> Lwt.return_unit
  | Some hs -> Lwt_list.iter_p (fun f -> f ev ctx) hs   (* parallel; no catching *)

let emit_async tbl ev ctx =
  Lwt.async (fun () -> emit tbl ev ctx)
