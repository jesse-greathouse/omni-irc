module Make (IO : Irc_sig.Io.S) = struct
  type conn = IO.t
  type cfg = {
    host : string;
    port : int;
    username  : string option;
    password  : string option;
    realname  : string option;
    charset   : string option;
    tls       : bool;
    keepalive : bool;
  }

  let connect (_:cfg) : conn Lwt.t =
    Lwt.fail_with "omni-irc-conn: connect not implemented (provide IO adapter)"

  let recv  (c:conn) (buf:bytes)   = IO.recv c buf
  let send  (c:conn) ?off ?len b   = IO.send c ?off ?len b
  let close (c:conn)               = IO.close c
end
