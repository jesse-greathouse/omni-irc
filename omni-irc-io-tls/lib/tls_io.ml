(* SPDX-License-Identifier: LicenseRef-OmniIRC-ViewOnly-1.0 *)
open Lwt.Infix

module Endpoint = struct
  type t = {
    host   : string;
    port   : int;
    sni    : string option;
    verify : bool;
    alpn   : string list;
  }

  let make ~host ~port ?sni ?verify ?alpn () =
    let verify = Option.value ~default:true verify in
    let verify =
      match Sys.getenv_opt "OMNI_TLS_INSECURE" with
      | Some "1" -> false
      | _ -> verify
    in
    { host; port; sni; verify; alpn = Option.value ~default:[] alpn }
end

let () = Mirage_crypto_rng_unix.use_default ()

let domain_host_of_string (h : string) : [ `host ] Domain_name.t option =
  match Domain_name.of_string h with
  | Ok dn -> (match Domain_name.host dn with Ok host -> Some host | Error _ -> None)
  | Error _ -> None

module IO : Irc_sig.Io.S with type endpoint = Endpoint.t and type t = Tls_lwt.Unix.t = struct
  type t = Tls_lwt.Unix.t
  type endpoint = Endpoint.t

  let connect (ep : endpoint) =
    Lwt_unix.getaddrinfo ep.host (string_of_int ep.port)
      [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ] >>= function
    | [] -> Lwt.fail_with ("omni-irc-io-tls: no addrinfo for " ^ ep.host)
    | ai :: _ ->
      let fd = Lwt_unix.socket ai.ai_family ai.ai_socktype ai.ai_protocol in
      Lwt_unix.connect fd ai.ai_addr >>= fun () ->

      (* authenticator: system roots if verify=true, else no verification *)
      let null_authenticator : X509.Authenticator.t =
        fun ?ip:_ ~host:_ _chain -> Ok None
      in

      let pick_authenticator () =
        if not ep.verify then Lwt.return null_authenticator
        else
          (* 1) Try OS/system roots *)
          let sys_auth =
            match Ca_certs.authenticator () with
            | Ok a -> Some a
            | Error (`Msg m) ->
                prerr_endline ("[tls] system CA load failed: " ^ m);
                None
          in
          match sys_auth with
          | Some a -> Lwt.return a
          | None ->
            (* 2) Fallback to Mozilla/NSS bundle *)
            match Ca_certs_nss.authenticator () with
            | Ok a -> Lwt.return a
            | Error (`Msg m2) ->
                Lwt.fail_with ("omni-irc-io-tls: no CA roots found (system/NSS): " ^ m2)
      in

      pick_authenticator () >>= fun authenticator ->

      let alpn_opt = if ep.alpn = [] then None else Some ep.alpn in
      let cfg =
        match Tls.Config.client ~authenticator ?alpn_protocols:alpn_opt () with
        | Ok c -> c
        | Error (`Msg m) ->
            failwith ("omni-irc-io-tls: Tls.Config.client: " ^ m)
      in

      let host_for_sni =
        match ep.sni with
        | Some s -> domain_host_of_string s
        | None   -> domain_host_of_string ep.host
      in
      Tls_lwt.Unix.client_of_fd cfg ?host:host_for_sni fd

  let recv (t : t) (buf : bytes) : int Lwt.t =
    Tls_lwt.Unix.read t buf

  let send (t : t) ?(off = 0) ?len (b : bytes) : int Lwt.t =
    let blen = Bytes.length b in
    let len  = match len with Some l -> min l (blen - off) | None -> blen - off in
    if off = 0 && len = blen then
      Tls_lwt.Unix.write t (Bytes.unsafe_to_string b) >|= fun () -> len
    else
      Tls_lwt.Unix.write t (Bytes.sub_string b off len) >|= fun () -> len

  let close = Tls_lwt.Unix.close
end
