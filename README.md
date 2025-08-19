# Omni-IRC

Omni-IRC is an experimental IRC client project written in [OCaml](https://ocaml.org/).  
It is designed as a modular learning project, with separate libraries for connection handling, input/output abstractions, and terminal user interfaces.

The current implementation demonstrates:

- **TCP + UNIX socket bridging** – The client connects to an IRC server and exposes a UNIX domain socket for communication.
- **Basic terminal UI** – Built with [notty](https://github.com/pqwy/notty) and [lwt](https://github.com/ocsigen/lwt) to display incoming messages and capture keyboard input.
- **Minimal IRC handshake** – Successfully opens a TCP connection and shows server notices. (The full IRC negotiation loop, such as `PING`/`PONG`, has not yet been implemented.)
- **Project structure** – Split into multiple OCaml libraries with `dune` and `.opam` files for modular development.

At this stage, the project is primarily a **learning and exploration tool** for OCaml programming, modular project layouts, and asynchronous I/O patterns.

---

## Project Layout

```sh

omni-irc/
├── dune-project
├── omni-irc-client.opam         # Client executable package
├── omni-irc-conn.opam           # Connection layer
├── omni-irc-io-unixsock.opam    # IO abstraction (UNIX sockets)
├── omni-irc-sig.opam            # Shared signatures
├── omni-irc-tui.opam            # Terminal UI wrapper
├── omni-irc-ui-notty.opam       # Notty-based UI implementation
└── omni-irc-client/
└── bin/main.ml              # Entry point

````

---

## Requirements

- OCaml (5.1+ recommended)
- dune (>= 3.0)
- opam (for package management)

Libraries used:

- [lwt](https://github.com/ocsigen/lwt) – concurrency
- [notty](https://github.com/pqwy/notty) – terminal UI
- [notty-lwt](https://github.com/pqwy/notty) – Lwt bindings for notty

---

## Building

Clone the repository and run:

```sh
opam switch create . ocaml-base-compiler.5.1.1   # or your preferred OCaml version
opam install . --deps-only
dune build
````

---

## Running

To start the client and connect to an IRC server:

```sh
dune exec --root . -- omni-irc-client \
  --server aries \
  --port 6667 \
  --nick tricky \
  --realname jessegreathouse \
  --socket /tmp/omni-irc-aries \
  --ui omni-irc-tui
```

The UI will show server notices and let you type input.
Currently supported controls:

```sh
--------------------------------
enter=\r\n       q/esc/ctrl-c=quit
```

---

## Current Limitations

- No IRC parser yet — output is shown raw from the server.
- No response handling (e.g. `PING` → `PONG`) — connections will time out after a short while.
- Only one UI backend (notty) implemented.
- No message history or scrolling.

---

## Roadmap

Planned improvements:

- Add a proper IRC protocol parser.
- Implement the handshake sequence (`NICK`, `USER`, `PONG`, etc.).
- Build modular UI panes for channels, private messages, and server notices.
- Add configuration file support.
- Provide multiple UI backends (curses, web, etc.).
