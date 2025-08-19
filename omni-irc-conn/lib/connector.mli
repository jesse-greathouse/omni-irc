module Make (IO : Irc_sig.Io.S) : sig
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

  val connect : cfg -> conn Lwt.t
  val recv    : conn -> bytes -> int Lwt.t
  val send    : conn -> ?off:int -> ?len:int -> bytes -> int Lwt.t
  val close   : conn -> unit Lwt.t
end
