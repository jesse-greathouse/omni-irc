(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module StringSet = Set.Make(String)

type t = {
  name   : string;
  topic  : string option;
  users  : StringSet.t;
  ops    : StringSet.t;
  voices : StringSet.t;
  modes  : string list;
}

let key_of_name (s : string) =
  let base =
    if s = "" then s else
    match s.[0] with
    | '#' | '&' -> String.sub s 1 (String.length s - 1)
    | _ -> s
  in
  String.lowercase_ascii base

let make ~name =
  { name; topic = None; users = StringSet.empty; ops = StringSet.empty; voices = StringSet.empty; modes = [] }

let set_topic t topic = { t with topic }

let set_modes t modes = { t with modes }
let add_mode  t m =
  if List.exists ((=) m) t.modes then t else { t with modes = t.modes @ [m] }
let remove_mode t m = { t with modes = List.filter ((<>) m) t.modes }

let add_user  t key = { t with users  = StringSet.add key t.users  }
let add_op    t key = { t with ops    = StringSet.add key t.ops    }
let add_voice t key = { t with voices = StringSet.add key t.voices }

let remove_user t u =
  { t with
    users  = StringSet.remove u t.users;
    ops    = StringSet.remove u t.ops;
    voices = StringSet.remove u t.voices;
  }

let remove_op    t u = { t with ops    = StringSet.remove u t.ops    }
let remove_voice t u = { t with voices = StringSet.remove u t.voices }

let has_user  t u = StringSet.mem u t.users
let has_op    t u = StringSet.mem u t.ops
let has_voice t u = StringSet.mem u t.voices

let remove_all t key =
  { t with
    users  = StringSet.remove key t.users;
    ops    = StringSet.remove key t.ops;
    voices = StringSet.remove key t.voices
  }

let wire_of_name s =
  if s = "" then s else
  match s.[0] with
  | '#' | '&' -> s
  | _ -> "#" ^ s

let to_wire t = wire_of_name t.name
