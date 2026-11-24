# nodecode-recued

Recued: your own notes, tasks and the things you own as tools the model can call. A Nodecode rewrite of [recued/agent-plugin](https://github.com/recued/agent-plugin) by [recued](https://github.com/recued).

Recued is a server you run on a computer you control. It keeps your notes, tasks, mail, calendar and the things you own, and it stands between that data and any AI: it checks every request against what you granted, holds a change for your approval, and keeps a record of what was done.

The original package was an [Agent Plugins](https://agent-plugins.org) folder with no code of its own: one line declaring Recued's MCP server, `recued --mcp`, and one skill telling the agent how to work with it. This cell carries the same two things into Nodecode: the server line, written into the config for you, and the same guidance as the manual topic `recued`. The talking is Nodecode's own `nodecode-mcp` cell, which turns each tool the server offers into a function in the `mcp` package.

## Get it

Tell the agent "add the recued cell", or run `nodecode add nc://recued`.

## Set it up

1. Recued 26.10.30 or later, installed and running on this computer, with the `recued` command on the `PATH` Nodecode starts with: [install it](https://recued.com/docs/setup/install-server/).
2. In the Recued web app, open Contracts, make a contract, choose the tools this app may use, and copy its key.
3. In a terminal on this computer, run `recued mcp-key set`, pick this app or "Every app", and paste the key. The key is saved in Recued, never in this cell and never in the config.
4. Restart Nodecode. The cell adds `mcp.servers.recued` at start; `/recued` says whether it is there, and `(restart-cells "nodecode-mcp")` starts it without a relaunch.

`recued mcp-key list` shows which contract each app uses, and `recued mcp-key clear` removes one.

Nothing here is required for the cell to load: with no Recued on the computer it still loads, `/recued` says the command is not on `PATH`, and the section's own settings are unchanged.

## Use it

- Ask for your notes, tasks or the things you own: the model calls the tools your contract grants, as `(mcp:call "recued" "work.search" ...)` or through the names `(mcp:tools "recued")` lists.
- Reading runs at once. A change waits for you: the tool answers `awaiting_approval` with an action reference, the change sits in the bell at the top of the Recued page, and the model asks for the result once you have answered. `(help :recued)` carries that whole routine, so the model does not have to be told twice.
- `/recued` answers one line: whether `mcp.servers` has the server, which command and arguments it runs, and whether that command is on this computer. Its panel connects it, takes it out again, and jumps to `/mcp recued` for the server's state and its tools.

## What is in it

| File | What it is |
| --- | --- |
| `nodecode-recued.asd` | The system, and the test system beside it. |
| `cell.lisp` | The `recued` section, the manual topic (the original skill, whole), and `/recued`. |
| `config.example.jsonc` | The section, documented; the declaration in `cell.lisp` is the authority. |
| `test/` | Offline tests: the entry it writes, the lines `/recued` answers, the refusals. Config and `PATH` are stubbed; no process starts. |

## Settings

The `recued` section of `~/.nodecode/config.jsonc`: `server_name` (default `recued`, the name the server and its tools appear under), `command` (default `recued`), `args` (default `["--mcp"]`), `timeout_seconds` (default 120, at least 10, one tool call's ceiling) and `auto_connect` (default true: write the server into `mcp.servers` at start when it is not already there, leaving one you wrote by hand exactly as it is).

## License

MIT, as the original. See `LICENSE` and `NOTICE`.
