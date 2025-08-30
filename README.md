# Omni-IRC

Omni-IRC is a modular, experimental IRC client written in [OCaml](https://ocaml.org/).
It’s structured as small libraries (IO backends, connector, engine, UI) plus a thin client executable, so you can mix and match pieces or reuse them in other repos.

## Highlights (current state)

* **Transport backends**

  * `omni-irc-io-tcp` (plain TCP via `lwt.unix`)
  * `omni-irc-io-tls` (TLS via `tls-lwt`, pure OCaml; CA verification via `ca-certs`)
  * `omni-irc-io-unixsock` (AF\_UNIX utility module; not used by the default client yet)
* **Connector functor** (`omni-irc-conn`): turns an IO backend into a uniform `{connect; recv; send; close}` API.
* **Event engine** (`omni-irc-engine`): tiny dispatcher + minimal parser.

  * Default handlers include **PING → PONG** and **INVITE** (auto-join + notify).
* **UI** (`omni-irc-ui-notty`): Notty/Lwt TUI with input line, scrollback, and simple keybinds.
* **One true entrypoint**: `bin/omni` (or `bin/omni.exe` on Windows) is built by Dune and promoted to the repo root `bin/` for easy use—no PATH edits required.
* **Build info**: `dune build @info` writes an absolute-paths manifest to `./omni-info.txt` for automation.

---

## Project layout

```sh
omni-irc/
├── bin/
│   ├── audit                 # helper script (unrelated to build)
│   └── dune                  # rules for building bin/omni (+ @info)
├── dune                      # root dune file (info rule, etc.)
├── dune-project
├── LICENSE
├── README.md
├── omni-irc-sig/             # tiny shared signatures
│   └── lib/
├── omni-irc-conn/            # connector functor over an IO backend
│   └── lib/
├── omni-irc-io-tcp/          # TCP backend
│   └── lib/
├── omni-irc-io-tls/          # TLS backend (ocaml-tls)
│   └── lib/
├── omni-irc-io-unixsock/     # AF_UNIX helper (not used by default client yet)
│   └── lib/
├── omni-irc-engine/          # event engine + minimal parser + core handlers
│   └── lib/
├── omni-irc-ui/              # UI contracts
│   └── lib/
├── omni-irc-ui-notty/        # Notty/Lwt UI implementation
│   └── lib/
└── omni-irc-client/          # thin client that wires it all together
    ├── bin/main.ml
    └── lib/
```

---

## Requirements

* OCaml **5.3+** (project is currently tested with 5.3.0)
* dune **3.20+**
* opam (recommended)
* For TLS: `tls-lwt`, `x509`, `ca-certs`, `domain-name`, `cstruct`
* For the TUI: `notty`, `notty.lwt` (Notty is POSIX-oriented)

Install deps:

```sh
opam switch create . ocaml-base-compiler.5.3.0   # or your preferred 5.x
opam install . --deps-only
```

---

## Build

### Build everything (normal Dune build)

```sh
dune build
```

### Build the user-facing executable

```sh
dune build @omni
# Result (POSIX): bin/omni
# Result (Windows): bin/omni.exe
```

### Generate absolute-path build info

```sh
dune build @info
cat omni-info.txt

# example (POSIX)
# context = default
# system  = linux
# ocaml   = 5.3.0
# exe     = /abs/path/to/_build/default/omni-irc-client/bin/main.exe
# omni_posix = /abs/path/to/bin/omni
# omni_win   = /abs/path/to/bin/omni.exe
```

> `omni-info.txt` is ignored by git.

---

## Run

Use the promoted wrapper in `bin/`:

```sh
# TLS (Libera.Chat example)
bin/omni \
  --server irc.libera.chat \
  --port 6697 \
  --tls \
  --nick yournick \
  --realname "Your Name"
```

Options:

* `--server <host>` (required)
* `--port <int>` (required)
* `--tls` (use TLS backend; otherwise plain TCP)
* `--nick <nick>` (optional, initial NICK)
* `--realname <name>` (optional, initial USER realname)
* `--ui <name>` (defaults to `notty`; currently only `notty` is implemented)

Windows:

```powershell
bin\omni.exe --server irc.libera.chat --port 6697 --tls --nick yournick --realname "Your Name"
```

---

## UI quick reference (Notty)

* **Enter**: send the typed line (CRLF appended)
* **Backspace**: delete one character
* **Up / Down**: scroll history
* **q** / **Q** / **Esc** / **Ctrl-C**: quit

---

## Engine & parsing

* Minimal parser recognizes:

  * `PING [:token]` → handler sends `PONG [:token]`
  * `INVITE <nick> :#channel` → handlers auto-join and show a notification
  * Everything else is surfaced as raw/other events
* Event dispatch is simple and non-blocking; add handlers by name in the engine.

---

## Development workflow

Common commands:

```sh
# Just the main executable under _build
dune exec omni-irc-client

# Build + promote the repo-root wrapper
dune build @omni

# Show build info with absolute paths
dune build @info && cat omni-info.txt

# Clean build artifacts
dune clean
```

---

## Roadmap

* Expand parser (RPLs, CTCP, numerics, message tags)
* More core handlers (e.g. JOIN/PART topic/mode notices)
* Config file + profiles
* Multiple UI backends (curses, web)
* Reintroduce/expand AF\_UNIX use as a pluggable IPC channel

---

## License

See `LICENSE` (project currently uses a custom “LicenseRef-OmniIRC-ViewOnly-1.0”).
