# Omni-IRC

Omni-IRC is a modular, experimental IRC client written in [OCaml](https://ocaml.org/). It’s split into small, composable libraries (IO backends, connector, engine, UI) plus a thin client executable. You can reuse the pieces independently or wire them together as a full terminal client.

📄 **Design & UI/Client contract:** see **[`doc/omni-irc-UI-Client-Contract.pdf`](doc/omni-irc-UI-Client-Contract.pdf)**  
🖼️ **Diagram illustration:** [![Omni-IRC Control Flow](https://i.imgur.com/kPQCqxU.png)](https://lucid.app/publicSegments/view/5788cf4b-75a6-4946-88b2-b305e7aa1c4c/image.png)

---

## What’s here (current architecture)

### Core packages

* **`omni-irc-sig`** — tiny shared signatures (notably the IO signature).
* **IO backends**

* **`omni-irc-io-tcp`** — plain TCP via `lwt.unix`.
* **`omni-irc-io-tls`** — TLS via `tls-lwt` (pure OCaml).
  * CA verification via `ca-certs` (enabled by default).
  * Works on Linux/macOS **and Windows** (no `ca-certs-nss` dependency required).
* **`omni-irc-io-unixsock`** — AF\_UNIX helper (simple single-consumer bridge).
* **`omni-irc-conn`** — **connector functor** that turns any IO backend into a uniform `{ connect; recv; send; close }` API.
* **`omni-irc-engine`** — parser + event dispatcher + default core handlers.

  * Recognizes and handles:

    * `PING` → `PONG`
    * `INVITE` → auto-join + notify
    * `LIST` (`322/323`) → channel list assembly
    * `NAMES` (`353/366`) → channel membership assembly
    * `TOPIC` (`332`) → channel topic tracking
    * `PRIVMSG` (to you) → surface as a notification line
    * `QUIT`/`KILL`/`JOIN`/`PART` → model updates
    * `MODE` (user & channel) → mode tracking
    * `WHOIS` (`311/312/319/338/671/318`) → enrich user model
* **`omni-irc-ui`** — UI contracts **and** the packaged loopback UI:
  * **`omni-irc-ui.loopback`** — TCP loopback UI library (for embedding / external GUIs).
* **`omni-irc-ui-notty`** — Notty/Lwt terminal UI (input line, scrollback, simple keybinds).
  * Not built on Windows (`os != win32`).
* **`omni-irc-client`** — thin orchestrator:

  * Wires IO ⇄ connector ⇄ engine ⇄ model
  * Bridges UI commands to client actions
  * Exports **state snapshots** to the UI via **`CLIENT [JSON]`** meta-lines (one-way, “React-ish” snapshot flow)

### Platform matrix (what builds where)

| Component                 | Linux/macOS | Windows |
|--------------------------|-------------|---------|
| TLS (`omni-irc-io-tls`)  | ✅           | ✅       |
| Notty UI (`omni-irc-ui-notty`) | ✅     | ❌       |
| AF_UNIX bridge (`omni-irc-io-unixsock`) | ✅ | ❌       |

### Models (authoritative on the client)

* **Users** — `Model_user.t`

  * Global modes, optional WHOIS cache, **per-channel modes**.
  * **Singular `self_user`** pointer identifies the connected user (see below).
* **Channels** — `Model_channel.t`

  * Users/ops/voices sets, topic, channel modes.
* **Channel List** — `Model_channel_list.t`

  * Map of `LIST` entries (`name`, `num_users`, `topic`).

### One-way state export to UI

The client emits **`CLIENT <json>`** lines to push *snapshots* of state to the UI. The UI keeps its own representation by consuming these snapshots. It **does not** re-emit state back; the UI only “pulls levers” by sending commands to the client.

Currently exported blobs include:

* `{"type":"channels","op":"snapshot" | "upsert" | "remove", ...}`
* `{"type":"channel",  "channel": { ... } }`
* `{"type":"chanlist", "entries": [ ... ] }`
* `{"type":"user","op":"upsert","user": { ... } }` — authoritative user upserts
* **`{"type":"client_user","op":"pointer"|"upsert", ...}`** — the *self user* (see below)

The Notty UI also accepts `CONSOLE <json>` as an alias for client JSON blobs.

---

## The “self user” flow

The client maintains a **single pointer** `self_user : User.t option` that references the authoritative `User.t` for the **connected identity**.

* On connect (when `--nick` is provided), the client:

  1. Sends `NICK`.
  2. Sets the pointer via `set_self_by_nick`.
  3. Emits:

     * a **pointer** message (fast):
       `{"type":"client_user","op":"pointer","key":"<normalized-nick>","nick":"<display>"}`
     * an **upsert** (snapshot):
       `{"type":"client_user","op":"upsert","key":"<normalized-nick>","user":{...}}`
* The UI stores the key, tries to dereference in its local cache, and shows a one-liner like:

  * `(self: jesse ident=… host=… account=… whois.secure=true)`
  * If details aren’t cached yet: `(self: jesse) (details pending)`
* You can trigger a refresh any time with the UI command **`/self`** (alias **`/whoami`**).

---

## Requirements

* **OCaml 5.3+** (tested with 5.3.0)
* **dune 3.20+**
* `opam` (recommended)
* TLS backend: `tls-lwt`, `x509`, `ca-certs`, `domain-name`, `mirage-crypto-rng`, `mirage-crypto-ec`
* Notty UI: `notty`, `notty.lwt` (POSIX-oriented; not available on Windows)

### Installing from source (recommended flow for consumers)

You can install from this repo via opam pin to pull the monorepo packages:

```sh
  opam pin add omni-irc git+https://github.com/jesse-greathouse/omni-irc.git#main -y 
  # or pin a tag once released: 
  opam pin add omni-irc git+https://github.com/jesse-greathouse/omni-irc.git#v0.1.15 -y
  # Then install the thin client (brings required libs): +opam install omni-irc-client
```

---

## Build

Build everything:

```sh
dune build
```

Run the main executable directly:

```sh
dune exec omni-irc-client
```

(If your environment promotes a wrapper to `bin/`, you can also run `bin/omni` / `bin/omni.exe`. Otherwise, `dune exec` is the simplest.)

Generate absolute-path build info (optional, used by tooling):

```sh
dune build @info
cat omni-info.txt
```

---

## Run

### UI selection & cross-platform behavior

There are two runtime-selectable UI adapters:

* notty — terminal UI (Linux/macOS). Not built on Windows.
* loopback — TCP loopback UI (all platforms). Good for embedding/external GUIs.

The --headless flag is a portable toggle:

* On Linux/macOS it selects the POSIX “headless” UI (AF_UNIX bridge).
* On Windows it is tantamount to --ui loopback.

Default loopback port:

* Windows: 58217
* Linux/macOS: 8765

Override with --socket <socket-file>.

### TLS example (Libera.Chat):

```sh
dune exec omni-irc-client -- \
  --server irc.libera.chat \
  --port 6697 \
  --tls \
  --nick yournick \
  --realname "Your Name"
```

Plain TCP:

```sh
dune exec omni-irc-client -- \
  --server irc.example.org \
  --port 6667 \
  --nick yournick
```

Options:

* `--server <host>` **required**
* `--port <int>` **required**
* `--tls` (enable TLS backend; uses ca-certs for verification by default)
* `--nick <nick>` (initial nick; also seeds the `self_user` pointer)
* `--realname <name>` (initial real name used in `USER`)
* `--ui <name>` choose notty (POSIX) or loopback (all platforms)
* `--headless` portable toggle (POSIX: AF_UNIX headless; Windows: maps to loopback)
* `--socket <port>` TCP port for loopback/headless UI bridge (defaults vary by OS; see above)

> TLS notes: certificate verification is on by default (via ca-certs). The client uses host-based SNI; override details in code if you need custom authenticators.

---

## Notty UI: quick reference

* **Enter**: send the typed line (CRLF appended)
* **Backspace**: delete one character
* **Up / Down**: scroll history
* **q / Q / Esc / Ctrl-C**: quit

**Slash commands**

These are parsed by the UI and forwarded to the client as typed commands:

* `/join <#ch>` — join channel (auto-adds `#` if missing)
* `/names [#ch]` — ask the server for names in a channel
* `/nick <newnick>` — change nick
* `/msg <target> <message…>` — send a `PRIVMSG`
* `/list [substr] [limit]` — request channel list (client gates and emits a table snapshot)
* `/raw <literal IRC line…>` — send as-is (+CRLF if missing)
* `/whois <nick>` or `/user <nick>` — WHOIS with caching
* `/channel <#ch>` — emit a one-channel snapshot blob to the UI
* **`/self`** or **`/whoami`** — re-emit a `client_user` upsert of the current self user

**What you’ll see**

* Raw IRC lines rendered to the scrollback.
* Client state snapshots rendered as compact tables/messages, e.g.:

  * `(chanlist: N entries total)` with a simple “Channel / Users / Topic” table
  * `(channel updating: #ocaml)` when membership/topics/modes change
  * `(Updating user nick: …)` when WHOIS/user data is upserted
  * `(self: …)` when pointer/upsert for the self user arrives

## Loopback UI: quick reference

The loopback UI exposes the same CLIENT JSON snapshots and accepts the same command lines over a local TCP socket.

* Default port: 58217 (Windows) / 8765 (Linux/macOS)
* Change with --socket [socket-file]
* Ideal for external GUIs (e.g. an Electron/React app) to connect and render state.

Example:

```sh
# Windows example, TLS + loopback UI on the default port: 
omni-irc-client.exe --server irc.libera.chat --port 6697 --tls --ui loopback
# or equivalently: 
omni-irc-client.exe --server irc.libera.chat --port 6697 --tls --headless
```

---

## How the pieces fit (mental model)

Think of the client as the **parent** and the UI as the **child**:

* The **UI “pulls levers”** (commands → `UiCmd`) like `/join`, `/whois`, `/self`.
* The **client owns the state** (users, channels, list) and **pushes snapshots** to the UI as `CLIENT [JSON]` lines whenever it changes (or on demand).
* The **UI never re-states** the model; it **renders** what the client exports and updates its local view.

This keeps concerns clean:

* IRC parsing, modeling, and correctness live in the client/engine.
* The UI remains reactive and stateless beyond its local cache and presentation.

For the exact JSON shapes, see the **UI/Client contract**:
👉 [`doc/omni-irc-UI-Client-Contract.pdf`](doc/omni-irc-UI-Client-Contract.pdf)

---

## Development workflow

Common commands:

```sh
# Run the client
dune exec omni-irc-client -- --server ... --port ... [--tls] [--nick ...] [--realname ...]

# Build + (if configured) promote wrapper
dune build @omni

# Absolute-path build info
dune build @info && cat omni-info.txt

# Clean build artifacts
dune clean
```

---

## Roadmap (near-term ideas)

* Emit `client_user` pointer automatically when a **server-observed nick change** occurs.
* Add a `/self whois` convenience (time-gated WHOIS refresh + upsert).
* Broaden parser/handlers (more numerics, message tags, CTCP).
* Additional UI backends (curses, web).
* Windows parity:
  * optional POSIX-feature shims where it makes sense,
  * more examples using the loopback UI (e.g. GUI demos).
* Expand AF\_UNIX usage for IPC/testing harnesses.

---

## License

See `LICENSE` (custom: **LicenseRef-OmniIRC-ViewOnly-1.0**).
