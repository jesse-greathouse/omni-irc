(lang dune 3.20)

(name omni-irc)
(version v0.1.12-1-g01e9475)

(generate_opam_files true)

(implicit_transitive_deps true)

(maintainers "Jesse Greathouse <jesse.greathouse@gmail.com>")
(authors "Jesse Greathouse")
(homepage "https://github.com/jesse-greathouse/omni-irc")
(bug_reports "https://github.com/jesse-greathouse/omni-irc/issues")
(documentation "https://github.com/jesse-greathouse/omni-irc#readme")
(license "LicenseRef-OmniIRC-ViewOnly-1.0")
(source (github jesse-greathouse/omni-irc))

(package
 (name omni-irc)
 (synopsis "Omni IRC: meta package for the monorepo")
 (description "Meta package for the Omni-IRC libraries. Install the specific subpackages: omni-irc-sig, omni-irc-conn, omni-irc-engine, omni-irc-io-{tcp,tls,unixsock}, omni-irc-ui, omni-irc-ui-notty (and optionally omni-irc-client).")
 (allow_empty)
 (depends
  (ocaml (>= 5.0))
  omni-irc-sig
  omni-irc-conn
  omni-irc-engine
  omni-irc-io-tcp
  (omni-irc-io-tls     (and (<> os "win32")))
  (omni-irc-io-unixsock (and (<> os "win32")))
  omni-irc-ui
  (omni-irc-ui-notty   (and (<> os "win32")))))

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
 (allow_empty)
 (depends
  lwt
  ;; TLS stack only on non-Windows
  (tls-lwt     (and (<> os "win32")))
  (x509        (and (<> os "win32")))
  (ca-certs    (and (<> os "win32")))
  (domain-name (and (<> os "win32")))
  omni-irc-sig))

(package
 (name omni-irc-conn)
 (synopsis "Omni IRC: transport connector (functor over IO)")
 (depends lwt omni-irc-sig))

(package
 (name omni-irc-io-unixsock)
 (synopsis "Omni IRC: single-consumer AF_UNIX bridge")
 (allow_empty)
 (depends lwt omni-irc-sig))

(package
 (name omni-irc-ui-notty)
 (synopsis "Omni IRC: Notty-Lwt User Interface")
 (allow_empty)
 (depends
  lwt
  (notty  (and (<> os "win32")))
  omni-irc-ui))

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
  omni-irc-engine
  omni-irc-io-tcp
  (omni-irc-io-tls       (and (<> os "win32")))
  (omni-irc-io-unixsock  (and (<> os "win32")))
  omni-irc-ui
  (omni-irc-ui-notty     (and (<> os "win32")))))
