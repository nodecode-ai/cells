# nodecode-agentplaybooks

AgentPlaybooks Tools: playbook skills, MCP servers and federated OpenAPI tools in your harness, through AgentPlaybooks' own `apb` CLI. A Nodecode rewrite of [matebenyovszky/agentplaybooks](https://github.com/matebenyovszky/agentplaybooks) by [matebenyovszky](https://github.com/matebenyovszky).

## Get it

Tell the agent "add the agentplaybooks cell", or run `nodecode add nc://agentplaybooks`.

## Set it up

Install AgentPlaybooks' CLI yourself; the cell runs it, never bundles it:

```sh
npm install -g @agentplaybooks/cli
apb --version
```

Then name the playbook you work with in `~/.nodecode/config.jsonc`. `playbook` is the one setting the cell cannot invent; the rest have defaults, and `target` is the harness the CLI writes for:

```jsonc
{
  "agentplaybooks": {
    "playbook": "<playbook-guid>",
    "target": "hermes",
    "profile": "",          // --profile=NAME, when the CLI should write a named profile
    "directory": ""         // the project the CLI runs in; the session's own otherwise
  }
}
```

Keys are not settings: the CLI keeps its own login (`/apb login`) and reads a playbook key from the environment the targeted profile runs with, so no secret is written to config or asked for in chat.

## Use it

Ask the agent to check, set up or connect a playbook, or type a command yourself:

- `/apb` with no arguments reports whether the CLI is installed and names the tools; `/apb login` and any other `apb` command run through it.
- `apb_doctor` is read-only: the CLI, the target, destination permissions and whether the key is present. `check` also probes the MCP connection with this process's key environment, without invoking any tool.
- `apb_setup` previews a playbook's guided setup — its skill trees restored into the project and its profile config merged — and runs it only with `apply`. `tools_only` skips the file restore and needs no login.
- `apb_connect` previews the playbook MCP connection, scoped to one playbook or `account` for the account-wide one.
- `apb_project` pulls a playbook's skills into the project, syncs them into the harness, or pushes the reviewed project config — each a preview until `apply`.

Every write is a preview until it is applied, and a failing CLI keeps its own exit code and stderr in the answer (a missing or rejected key exits 2 after files were installed). The model reads the full guidance with `(help :agentplaybooks)`.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The declaration: four tools, `/apb`, the `(help :agentplaybooks)` topic, the argv the CLI gets. |
| `cli.lisp` | The one seam to the CLI: one argv in, its own output out, cut at `max_chars`. |
| `config.example.jsonc` | The section, commented. |
| `test/cell-test.lisp` | Offline tests: every `apb` call stubbed. |

## License

MIT. The original plugin is Copyright (c) 2026 AgentPlaybooks: see `LICENSE` and `NOTICE`.
