(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

let key_of_name = Model_channel.key_of_name
let wire_of_name = Model_channel.wire_of_name

type entry = {
  name      : string; (* normalized; no leading '#'/'&' *)
  num_users : int;
  topic     : string option;
}

module Map = Map.Make(String)
type t = entry Map.t

let empty = Map.empty

let find name m =
  let k = key_of_name name in
  Map.find_opt k m

let upsert ~name ~num_users ~topic m =
  let k = key_of_name name in
  let e = { name = k; num_users; topic } in
  Map.add k e m
