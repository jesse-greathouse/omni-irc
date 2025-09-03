(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)

let normalize_nick (s : string) =
  let s =
    match s with
    | "" -> s
    | _ ->
      (match s.[0] with
        | '@' | '+' | '%' | '&' | '~' -> String.sub s 1 (String.length s - 1)
        | _ -> s)
  in
  String.lowercase_ascii s

let key_of_nick s = normalize_nick s

type whois = {
  user        : string option;
  host        : string option;
  realname    : string option;
  server      : string option;
  server_info : string option;
  account     : string option;
  channels    : string list;
  idle_secs   : int option;
  signon_ts   : float option;
  actual_host : string option;
  secure      : bool option;
}

module Mode = struct
  let operator_mode  = "o"
  let voice_mode     = "v"
  let halfop_mode    = "h"
  let owner_mode     = "q"
  let admin_mode     = "a"
  let invisible_mode = "i"
end

module StringMap = Map.Make(String)

type t = {
  mutable nick      : string;
  mutable real_name : string option;
  mutable ident     : string option;
  mutable host      : string option;
  mutable account   : string option;
  mutable away      : bool option;
  mutable whois     : whois option;
  mutable modes     : string list;
  mutable channel_modes : string list StringMap.t;
}

let make ?real_name ?ident ?host ?account ?away ?whois
          ?(modes=[]) ?(channel_modes=StringMap.empty) nick =
  { nick; real_name; ident; host; account; away; whois; modes; channel_modes }

module Ord = struct
  type nonrec t = t
  let compare a b =
    Stdlib.compare (normalize_nick a.nick) (normalize_nick b.nick)
end

module Set = Set.Make(Ord)
