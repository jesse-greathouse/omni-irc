(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

module User = Model_user

type t = {
  name   : string;  (* normalized; no leading '#'/'&' *)
  topic  : string option;
  users  : User.Set.t;
  ops    : User.Set.t;
  voices : User.Set.t;
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
  { name; topic = None; users = User.Set.empty; ops = User.Set.empty; voices = User.Set.empty }

let set_topic t topic = { t with topic }

let add_user  t u = { t with users  = User.Set.add u t.users  }
let add_op    t u = { t with ops    = User.Set.add u t.ops    }
let add_voice t u = { t with voices = User.Set.add u t.voices }

let remove_user t u =
  { t with
    users  = User.Set.remove u t.users;
    ops    = User.Set.remove u t.ops;
    voices = User.Set.remove u t.voices;
  }

let remove_op    t u = { t with ops    = User.Set.remove u t.ops    }
let remove_voice t u = { t with voices = User.Set.remove u t.voices }

let has_user  t u = User.Set.mem u t.users
let has_op    t u = User.Set.mem u t.ops
let has_voice t u = User.Set.mem u t.voices

let wire_of_name s =
  if s = "" then s else
  match s.[0] with
  | '#' | '&' -> s
  | _ -> "#" ^ s

let to_wire t = wire_of_name t.name
