## v0.1.1 (2025-09-06)

- Initial public multi-package release of Omni-IRC libraries.
- Packages: omni-irc-sig, omni-irc-conn, omni-irc-engine, omni-irc-io-{tcp,tls,unixsock},
  omni-irc-ui, omni-irc-ui-notty, omni-irc-client (example CLI).

## v0.1.6 (2025-09-08)

- Updated Publishing Info

## v0.1.9 (2025-09-08)

- Added **first-class Windows (Win32) build support**:
  - Split client entrypoints:
    - `main_unix.ml` for Unix (TLS, Notty, AF_UNIX headless).
    - `main_win32.ml` for Windows (TCP + loopback UI).
  - Dune `enabled_if` stanzas now select the proper entrypoint per OS.
  - `bin/omni` (Unix) and `bin/omni.exe` (Windows) wrappers updated.
- Gated non-portable dependencies in `dune-project` and opam files:
  - `omni-irc-io-tls`, `omni-irc-ui-notty`, and `omni-irc-io-unixsock` build only on non-Windows.
  - `(allow_empty)` added to `omni-irc-io-tls` package for Windows compatibility.
- Cleaned up old root `dune` and redundant `.unix` dune fragments.
- **Result:** Windows builds now succeed; developers can connect to IRC on Win32
  via TCP and the loopback UI (`telnet 127.0.0.1 8765`).

## v0.1.11 (2025-09-08)

- **Windows packaging & CI fixes**
  - Ensure `omni-irc-ui-notty` and `omni-irc-io-unixsock` are **marked unavailable on Windows** in opam (`{ { os-family != "windows" } }`), so the solver skips them instead of failing the build.
  - Keep client installable on Windows via the loopback UI only; TLS and Notty remain non-Windows features.

- **Client build split**
  - Separate Unix and Windows entrypoints (`main_unix` / `main_win32`) to avoid conditional runtime wiring (TLS/Notty/Unix-socket vs. loopback TCP on Windows).

- **Opam/release housekeeping**
  - Consistent release asset naming (`omni-irc-<ver>.tbz`) referenced by all subpackages.
  - Minor metadata cleanups and dependency graph tightening for smoother pin/install.

_No user-facing protocol changes; this is a packaging/build reliability release._

## v0.1.12 (2025-09-09)

- **Removed cstruct dependency**
  - @hannesm pointed out [in this post](https://github.com/jesse-greathouse/omni-irc/issues/1) That the ocaml-tls library had not used cstruct since 1.0.0  (released August 2024)
