# nodecode-lsp

A Nodecode cell that puts language servers behind the model's `eval`. With
it, an eval that writes a file through `edit` or `write-file` ends with what
the servers say about that file:

```
wrote src/a.py: 5 lines, 64 characters
LSP diagnostics (1 error):
src/a.py:5:11 [error] [Pyright] Type "int" is not assignable to declared type "str" (reportAssignmentType)
```

and the model gets a few verbs in the `lsp:` package:

```lisp
(lsp:diagnostics "src/a.rs")                     ; every diagnostic, or OK; a list of paths for several
(lsp:definition "src/a.rs" "parse" :line 40)     ; where it is defined, with the line
(lsp:references "src/a.rs" "parse")              ; every use, the declaration included
(lsp:hover "src/a.rs" "parse")                   ; its type and docs
(lsp:symbols "src/a.rs")                         ; the file's outline; :query "Parser" searches the project
(lsp:rename "src/a.rs" "parse" "parse_all")      ; renames across files and writes them; :apply nil previews
(lsp:status)  (lsp:restart)                      ; what runs, and starting over
(lsp:request "rust-analyzer" "rust-analyzer/expandMacro" :params p :path "src/a.rs")
```

A symbol is found in the file by name: its first mention, `"parse#2"` the
second, `:line N` on that line. `(help :lsp)` is the model's manual.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `lsp` module:
its 55 server definitions (`servers.json`, omp's `defaults.json`), its
server selection and root markers, its diagnostics wait and output format.
See [NOTICE](NOTICE).

## How it runs

- A server starts the first time a file it serves is written or asked about,
  from the project's own bins (`node_modules/.bin`, `.venv/bin`, ...) or
  `PATH`, in the project root its markers name: the outermost directory
  holding one inside the session's repository, else the nearest. Every
  session working there shares it. It stops after `idle_minutes` unused, or
  when the cell stops.
- A file gets every server whose file types name it, type checkers before
  linters. Diagnostics after a write come from all of them; the verbs ask the
  first.
- The diagnostics after a write wait at most `wait_ms` (3 s) for the
  servers. A server that has not answered by then says
  `lsp: NAME still checking FILE; (lsp:diagnostics "FILE") for the result`.
  At most 50 diagnostics and 4,000 characters, errors and warnings only, the
  first line of each message. A clean file adds nothing.
- A verb waits at most 8 s, inside the eval's ten-second yield window; a
  project still loading answers `still indexing; try again`.
- A server's stderr goes to `~/.nodecode/lsp/NAME.log`. A server that fails
  to start says why, with the last lines it printed, and is tried again 30 s
  later.

Nothing here edits the core: the cell hooks `write-file-text`, the one
atomic writer under `edit` and `write-file`, and the `:tool` point.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-lsp/` and restart
Nodecode, or run `(restart-cells)`. Install the language servers you want
the usual way (`npm i -g pyright`, `rustup component add rust-analyzer`, ...);
the cell starts only what it finds.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "lsp": {
    "diagnostics_on_write": true,   // append diagnostics to an eval that wrote files
    "wait_ms": 3000,                // how long that waits for them
    "idle_minutes": 15,             // stop a server unused this long
    "servers": {
      // override one of the 55 by name: any member, the rest kept
      "pyright": { "command": "/opt/pyright/bin/pyright-langserver" },
      "ruff": { "disabled": true },
      // or add one
      "my-ls": { "command": "my-ls", "args": ["--stdio"],
                 "file_types": [".xyz"], "root_markers": [".git"],
                 "settings": {}, "init_options": {} }
    }
  }
}
```

`config.example.jsonc` has the whole section.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-lsp
```

The tests drive `test/fake-server.py`, a scripted language server, through
python3; nothing touches the network.

## Gaps

What omp's module does that v1 does not:

- **Format on write, code actions, `rename_file`.** omp formats a written file
  through the server, offers quick fixes and import organizing, and renames a
  file with the server updating its importers. A rename here edits text only:
  one that would also create, rename or delete a file is refused.
- **Late diagnostics.** omp waits 500 ms inline and delivers a slow server's
  diagnostics into the next turn. Here the write waits `wait_ms` and a slow
  server says "still checking"; the model asks again with
  `lsp:diagnostics`.
- **The biome and swiftlint clients, and `file: "*"` checkers.** omp drives
  those two through their CLIs rather than LSP, and can check every file a
  glob names; the cell skips both servers and checks named files.
- **The diagnostics dedup ledger.** omp suppresses a diagnostic it already
  showed for an unchanged file; here every write reports what stands.
- **The mux daemon and lspmux.** Not needed: the gateway is one long-lived
  process, so a server is already shared by every session in its root.

MIT licensed.
