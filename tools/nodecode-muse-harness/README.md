# nodecode-muse-harness

Muse Harness: runs one prompt inside a local Muse Code session and answers its reply. A Nodecode rewrite of [SnoobieJunes/hermes-muse-harness](https://github.com/SnoobieJunes/hermes-muse-harness) by [SnoobieJunes](https://github.com/SnoobieJunes).

Muse Code is Meta's coding harness. It ships a `muse` executable that serves a session over MSP, its own JSON-RPC protocol on stdin and stdout. This cell starts that executable, asks one turn, and hands the answer back to Nodecode as a tool result or a slash command. It is not a model provider: the model stays Muse's, inside Muse.

## Get it

Tell the agent "add the muse-harness cell", or run

```
nodecode add nc://muse-harness
```

## Set it up

Install Muse Code and run `muse login` once, so the executable is on `PATH` and holds a browser login. Nothing else is required.

The section, `muse-harness`, all optional:

| Setting | Default | Meaning |
|---|---|---|
| `binary` | `muse` | the executable: a name on `PATH`, or a path |
| `model` | `muse-wonder-full-1.0` | the Muse model a turn runs on when the call names none |
| `approval_mode` | `denyUnmatched` | `allowAll` lets the Muse session run its own shell and file tools inside the session directory without asking |
| `timeout_seconds` | `600` | how long one turn waits |
| `api_key_env` | unset | the variable holding a Meta Model API key, used only for the pay-as-you-go retry below |

`api_key_env` names a variable, never holding the key itself; it is the only setting that costs anything, and leaving it unset keeps every turn on the Muse login.

## Use it

The tool `muse_turn` runs one turn: `prompt`, and optionally `model`, `workspace`, `mcp_servers` and `timeout_seconds`. `/muse PROMPT` runs the same turn from the keyboard, and `/muse` alone prints the mode it would run under.

Billing is subscription first: the first attempt passes no key, so Muse answers on the browser login. When Muse reports, in its typed `error.quota` object, that the subscription allowance is exhausted, the same prompt is retried once on pay-as-you-go with `META_API_KEY` set from the variable `api_key_env` names. The answer says which one it was.

The rules the original set, kept:

- the child process gets an allowlist of the environment (`PATH`, `HOME`, `USER`, `TMPDIR`, `LANG`, `LC_*`, `XDG_*`), never provider keys, and never a subscription token another tool minted
- `mcp_servers` is a JSON object of loopback HTTP servers; a stdio command is refused, because a model-supplied command is execution
- the workspace must stay inside the session directory
- contributor mode is always off
- nothing here mints a key, calls `api.meta.ai`, or reads Muse's login files

```json
{"unreal":{"transport":"streamableHttp","url":"http://127.0.0.1:8000/mcp"}}
```

## Licence

MIT, the original's. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
