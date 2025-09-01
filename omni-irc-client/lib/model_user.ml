(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

type t = { nick : string }

let make nick = { nick }

let normalize_nick (s : string) =
  let s =
    match s with
    | "" -> s
    | _ ->
      begin match s.[0] with
      | '@' | '+' -> String.sub s 1 (String.length s - 1)
      | _ -> s
      end
  in
  String.lowercase_ascii s

module Ord = struct
  type nonrec t = t
  let compare a b =
    Stdlib.compare (normalize_nick a.nick) (normalize_nick b.nick)
end

module Set = Set.Make(Ord)
