(lang dune 3.11)
(name omni_irc)
(generate_opam_files true)
(implicit_transitive_deps true)

(package
 (name omni-irc-sig)
 (synopsis "Omni IRC: tiny IO signature")
 (depends lwt))

(package
 (name omni-irc-conn)
 (synopsis "Omni IRC: transport connector (functor over IO)")
 (depends lwt omni-irc-sig))

(package
 (name omni-irc-io-unixsock)
 (synopsis "Omni IRC: single-consumer AF_UNIX bridge")
 (depends lwt))

(package
 (name omni-irc-ui-notty)
 (synopsis "Omni IRC: Notty-Lwt TUI")
 (depends lwt notty))

(package
 (name omni-irc-tui)
 (synopsis "Omni IRC: demo TUI executable (Notty-Lwt)")
 (depends lwt notty))

(package
 (name omni-irc-client)
 (synopsis "Omni IRC: simple raw client (TCP<->AF_UNIX bridge + optional UI)")
 (depends lwt omni-irc-io-unixsock omni-irc-conn))
