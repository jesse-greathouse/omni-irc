(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module Make (P : Parser_intf.ENGINE) = struct
  type event = P.event
  type 'ctx handler = event -> 'ctx -> unit Lwt.t
  type 'ctx t = (string, 'ctx handler list) Hashtbl.t

  module Acc = struct
    type t = P.Acc.t
    let create     = P.Acc.create
    let push_chunk = P.Acc.push_chunk
  end

  let create () = Hashtbl.create 32

  let on tbl name h =
    let prev = match Hashtbl.find_opt tbl name with Some hs -> hs | None -> [] in
    Hashtbl.replace tbl name (prev @ [h])

  let emit tbl ev ctx =
    match Hashtbl.find_opt tbl (P.name ev) with
    | None -> Lwt.return_unit
    | Some hs ->
        Lwt_list.iter_s
          (fun f -> Lwt.catch (fun () -> f ev ctx) (fun _ -> Lwt.return_unit))
          hs

  let emit_async tbl ev ctx =
    Lwt.async (fun () -> emit tbl ev ctx)

  let ingest_line tbl line ctx =
    let ev = P.of_line line in
    emit tbl ev ctx

  let ingest_chunk tbl acc chunk ctx =
    let events = Acc.push_chunk acc chunk in
    Lwt_list.iter_s (fun ev -> emit tbl ev ctx) events
end
