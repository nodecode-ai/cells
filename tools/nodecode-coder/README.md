# nodecode-coder

Coder: run a command inside your own remote Coder workspace, and list or read those workspaces. A Nodecode rewrite of [perqin/hermes-plugin-coder](https://github.com/perqin/hermes-plugin-coder) by [perqin](https://github.com/perqin).

The original replaced the agent's own terminal with a [Coder](https://coder.com/) workspace, so every command ran there instead of on the machine you sit at. A cell has no terminal to replace, so the same reach is three tools the model calls:

| Tool | What it does |
| --- | --- |
| `coder_workspaces` | The workspaces you own, with each one's last build, status and template. |
| `coder_workspace` | One workspace in full: its build, and each agent's status and lifecycle state. |
| `coder_exec` | Runs one shell command in a workspace and answers its output and exit code. |

Ask "what is the build saying in my dev workspace?", "run the tests in `data` and tell me what failed", "is my workspace agent up?" — the model picks the workspace with `coder_workspaces`, and runs the command with `coder_exec`.

## Get it

Tell the agent "add the coder cell", or run `nodecode add nc://coder`.

## Set it up

You need a Coder deployment you can reach over HTTPS (yours, or your organisation's) and an existing workspace in it. This cell does not create workspaces; it uses one.

1. **A session token.** In Coder: your avatar, **Account**, **Security**, **Session token**, and copy it. Put it in an environment variable, e.g. `CODER_API_KEY`, and set `token_env` to that name; or save it in a file and set `token_file` to the path.
2. **The deployment's URL**, scheme included and nothing after the host: `https://coder.example.com`.
3. **A workspace name**, e.g. `my-workspace`: the one `coder_exec` runs in when a call names none, and the one to check first with `coder_workspaces`.

Optional: `forward_env` lists environment variable names whose values are copied into the shell for one command (never the session token itself), `startup_timeout` bounds how long a stopped workspace may take to start, and `timeout` bounds one command.

## Use it

```
coder_exec {"command": "git -C ~/app status --short"}
coder_exec {"command": "pytest -q", "workspace": "data", "timeout": 600}
```

A stopped workspace is started first, which can take minutes; the answer says so with `started`. The command runs as `bash -lc` inside the workspace, so it sees that workspace's files, tools and network, and its exit code is the workspace shell's own. The command rides the PTY's query string, so keep it under a few thousand characters: for anything longer, write a script inside the workspace and run the script.

The tools are the `coder` package's functions, so the same thing from eval:

```lisp
(coder:run-command (nlk:contract-decode (nlk:contract 'nodecode-coder::command-run)
                                         (nlk:decode-json "{\"command\":\"ls\"}")))
```
