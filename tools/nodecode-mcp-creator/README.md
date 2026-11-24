# nodecode-mcp-creator

MCP Plugin Creator: an MCP server's tools, written down as a skill a later session reads. A Nodecode rewrite of [tonbistudio/hermes-mcp-plugin-creator](https://github.com/tonbistudio/hermes-mcp-plugin-creator) by [tonbistudio](https://github.com/tonbistudio).

Point it at a server your profile already configures, and it reads the catalog, reads what you actually called, and writes a knowledge cell: a `define-skill` naming every tool with its parameters, the pitfalls your own failures taught it, and a `manifest.json` whose fingerprint says which catalog the skill was written against. A later `drift` tells you what the server changed underneath it.

## Get it

Tell the agent "add the mcp-creator cell", or run `nodecode add nc://mcp-creator`.

## Set it up

No key. The servers are the ones `~/.nodecode/config.jsonc` already gives the `nodecode-mcp` cell:

```jsonc
{
  "mcp": {
    "servers": {
      "files": { "command": "npx", "args": ["-y", "@modelcontextprotocol/server-filesystem", "~/docs"] },
      "docs":  { "url": "https://docs.example.com/mcp" }
    }
  }
}
```

The `mcp-creator` section only moves things around: `data_dir` (snapshots, builds and the call log, default `~/.nodecode/mcp-creator`), `install_dir` (default this home's `cells` folder), `timeout_seconds` (60), `max_desc` (220 characters per tool description) and `log_calls` (on). `config.example.jsonc` lists them with what each changes.

## Use it

Say "write a skill for the files server", or call the tools yourself:

| Tool | What it does |
| --- | --- |
| `mcp_creator_servers` | What this profile configures and what has been built from it. Start here. |
| `mcp_creator_introspect` | One read-only connection: server info, tools with their schemas, prompts, resources, and the fingerprint drift compares against. |
| `mcp_creator_mine` | Per-tool call and error counts, error samples, and which tool a call followed — the pitfalls a skill needs. |
| `mcp_creator_convert` | One entry as a portable `mcp.json` entry: `${VAR}` secrets, and what a publish build must fix. |
| `mcp_creator_scaffold` | Writes the knowledge cell: `nodecode-<name>.asd`, `skill.lisp`, `SKILL.md`, `mcp.json`, `manifest.json`. |
| `mcp_creator_validate` | Checks the manifest, the system name, the skill, the fingerprint, and any private detail a publish build still carries. |
| `mcp_creator_install` | Copies a build into the cells folder; the next boot loads it. |
| `mcp_creator_drift` | Added, removed and schema-changed tools, and a verdict: unchanged, additive or recompile. |
| `mcp_creator_review` | Drift plus usage since the build, and whether a refresh is worth it. |

`/mcp-creator` lists the servers and the builds and runs `introspect`, `drift` and `review` from a shell; `(help :mcp-creator)` carries the same rules while the cell runs, and the verbs are callable from eval as `(mcp-creator:drift "files")`.

What it keeps from the original:

- **Read-only introspection.** `initialize`, `tools/list`, `prompts/list`, `resources/list`, then close — including the child process. It never calls a tool.
- **Usage, not guesses.** A `:TOOL` hook records the MCP calls an eval form makes, redacted (credentials, emails, and paths when asked), one JSON line each, locally. `mine` folds that log.
- **A baseline with a fingerprint.** The snapshot `introspect` saves is what `drift` compares against, so "the tool's schema changed" is a fact about a hash, not a feeling.
- **Marker-gated builds.** `scaffold` will only replace a folder carrying its own `.mcp-creator` marker, and `install` refuses a folder it did not make unless you say `replace`.
- **A publish build that carries no secrets.** A credential-shaped value, an email or (when asked) an absolute path in a generated file is a finding, and a `publish` build fails on any.

## What the rewrite does not do

- **No `plugin.json` + `mcp.json` package.** The unit it builds is a Nodecode knowledge cell: an `.asd`, a prose `define-skill` the index lists, `SKILL.md` beside it, and `mcp.json` for any other MCP client. Nodecode reads the folders, not a plugin manifest.
- **No `hermes plugins validate|enable` subprocess.** `validate` is the cell's own structural check, and `install` copies the folder in; the boot loader is what loads a cell.
- **No approval gate.** The original asked before starting a server inline or writing outside its data directory; Nodecode's own tool prompts and the section are that gate here.
- **No `hermes mcp-creator` CLI.** The tools, `/mcp-creator`, `/settings` and `(cells)` are the surface.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-mcp-creator
```

Every exchange is stubbed at the cell's one connection seam (`NODECODE-MCP-CREATOR::*CONNECT*` answers a scripted catalog), so no test starts a process or opens a socket. The tests prove the folder starts and stops cleanly, that a section value it cannot use refuses the start, that a configured server is listed with its env names and not its secrets, that introspection, scaffolding, validation, installation and drift run end to end over a fake server, that mining counts what the hook recorded, and that an unknown server and a missing snapshot are refusals the model can repair from.

MIT licensed. See `NOTICE` for what came from the original and what did not.
