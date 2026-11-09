# nodecode-deepinfra-sandbox

DeepInfra sandboxes: run shell commands and keep files in an isolated cloud microVM instead of the user's machine. A Nodecode rewrite of [deepinfra/deepinfra-hermes-sandbox](https://github.com/deepinfra/deepinfra-hermes-sandbox) by [deepinfra](https://github.com/deepinfra).

## Get it

Tell the agent "add the deepinfra-sandbox cell", or run `nodecode add nc://deepinfra-sandbox`.

## Set it up

You need a DeepInfra account and an API key from
[deepinfra.com/dash/api_keys](https://deepinfra.com/dash/api_keys) — the same
account key the models use, with the `compute:sandbox` scope. Either export
it:

```sh
export DEEPINFRA_API_KEY=...
```

or set it in `/settings` (or the page's Settings tab) under
`deepinfra-sandbox`: `api_key` for the key itself, or `api_key_env` for the
name of the variable holding it. A key pasted into the panel is kept in a
0600 file under `~/.nodecode/secrets/`; the folder holds no secret.

Nothing has to be installed: the cell speaks DeepInfra's sandbox REST API
itself, at `https://api.deepinfra.com`. `/deepinfra` with no argument says
what the sandbox is, and `(help :deepinfra-sandbox)` is the model's guide.

Two settings are worth knowing before the first command:

- `plan` — the sandbox tier (`small`, `large`, …). Unset is the service's
own default, `medium`, which is not the cheapest.
- `idle_minutes` — how long an idle sandbox lives before the service stops
it. `/workspace` is kept either way, and a stopped sandbox costs nothing.

## Use it

Ask for what you want; the model picks the tool:

| Tool | Use |
| --- | --- |
| `deepinfra_run` | one shell command in the sandbox, answered with its output and exit code |
| `deepinfra_read` | a file's text out of `/workspace` |
| `deepinfra_write` | a whole file into `/workspace`, folders made |
| `deepinfra_sandbox` | `status` (dials nothing), `create`, `start`, `stop`, `delete` |

From the keyboard, `/deepinfra` shows the sandbox and `/deepinfra status`,
`create`, `start`, `stop` or `delete` runs that one action. From `eval`,
`(deepinfra-sandbox:exec "ls -l /workspace")` runs a command and answers
what it printed.

One sandbox belongs to one home: the first command makes it, and its id is
kept in `deepinfra-sandbox.id` beside the config so a restart resumes it. On
a stop it is stopped (or, with `keep_workspace` false, deleted) — a stopped
sandbox keeps `/workspace` and is not billed.

Only `/workspace` survives a stop/start cycle. A package installed outside
it, a file in `/tmp`, a background process: gone on the next start, which is
why the file verbs refuse a path outside `/workspace` and why the primer
tells the model to keep what matters there. The sandbox's environment does
not carry the DeepInfra key.

### What the original did that this does

The plugin was a Hermes `terminal.backend`: with it enabled, every shell and
file call of the harness went to a DeepInfra sandbox instead of the local
machine, one sandbox per task, resumed by tag after an idle reap, with the
harness's credential, skill and cache files pushed to `/workspace/.hermes`.
A Nodecode cell cannot take the core's shell away, so this one gives the
model four verbs of its own on the same service, over the same account key —
the same commands, the same `/workspace`, the same stop-and-resume — and
speaks the REST API the Python SDK wrapped, with `nlk:http` and nothing to
install.

## Not ported

- **Replacing the local shell.** The original swapped the whole terminal
backend; here the sandbox is a set of tools beside the local ones, so the
model chooses. Nothing of the user's filesystem is mounted in, as before.
- **The credential and skill sync.** The original pushed the harness's
`~/.hermes` credentials, skills and cache to `/workspace/.hermes` on every
construction and pulled the changed ones back. Here nothing of the harness's
state is uploaded: the sandbox gets what `deepinfra_write` puts there.
- **Per-turn resume by tag.** One sandbox per home is remembered by id in a
small file, which covers the restart and idle-reap case the tags covered.
- **Interrupt.** The original stopped the whole sandbox to cancel a command.
Here a command ends at its own timeout, up to the service's 1800-second cap.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-deepinfra-sandbox
```

Every exchange is a stubbed `nlk:http`, so nothing reaches DeepInfra and no
key is needed.

MIT licensed, as the original is.
