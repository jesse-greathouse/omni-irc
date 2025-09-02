(lang dune 3.20)

(name omni_irc)

(generate_opam_files true)

(implicit_transitive_deps true)

(maintainers "Jesse Greathouse <jesse.greathouse@gmail.com>")
(authors "Jesse Greathouse")
(homepage "https://github.com/jessegreathouse/omni-irc")
(bug_reports "https://github.com/jessegreathouse/omni-irc/issues")
(source (github jessegreathouse/omni-irc))
(documentation "https://github.com/jessegreathouse/omni-irc#readme")
(license "LicenseRef-OmniIRC-ViewOnly-1.0")

(package
 (name omni-irc-sig)
 (synopsis "Omni IRC: tiny IO signature")
 (depends lwt))

(package
 (name omni-irc-io-tcp)
 (synopsis "Omni IRC: TCP IO")
 (depends lwt omni-irc-sig))

(package
 (name omni-irc-io-tls)
 (synopsis "Omni IRC: TLS IO")
 (depends
  lwt
  tls-lwt
  x509
  ca-certs
  domain-name
  cstruct
  omni-irc-sig))

(package
 (name omni-irc-conn)
 (synopsis "Omni IRC: transport connector (functor over IO)")
 (depends lwt omni-irc-sig))

(package
 (name omni-irc-io-unixsock)
 (synopsis "Omni IRC: single-consumer AF_UNIX bridge")
 (depends lwt omni-irc-sig))

(package
 (name omni-irc-ui-notty)
 (synopsis "Omni IRC: Notty-Lwt TUI")
 (depends lwt notty yojson omni-irc-ui))

(package
 (name omni-irc-ui)
 (synopsis "Omni IRC: contracts for UI modules")
 (depends lwt))

(package
 (name omni-irc-engine)
 (synopsis "Omni IRC: event engine (dispatcher + core handlers)")
 (depends lwt))

(package
 (name omni-irc-client)
 (synopsis "Omni IRC: simple raw client (TCP<->AF_UNIX bridge + optional UI)")
 (depends
  lwt
  yojson
  omni-irc-sig
  omni-irc-conn
  omni-irc-io-unixsock
  omni-irc-io-tcp
  omni-irc-io-tls
  omni-irc-ui
  omni-irc-ui-notty))
