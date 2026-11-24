# nodecode-boxcompute

BoxCompute: cloud Linux VMs — create one, run commands, edit files, share a public preview link. A Nodecode rewrite of [boxcompute/hermes-plugin](https://github.com/boxcompute/hermes-plugin) by [boxcompute](https://github.com/boxcompute).

## Get it

Tell the agent "add the boxcompute cell", or run `nodecode add nc://boxcompute`.

## Set it up

You need a BoxCompute account and a key from the app, under **Menu → API
keys**. Either export it:

```sh
export BOXCOMPUTE_API_KEY=bxc-...
```

or set it in `/settings` (or the page's Settings tab) under `boxcompute`:
`api_key` for the key itself, or `api_key_env` for the name of the variable
holding it. A key pasted into the panel is kept in a 0600 file under
`~/.nodecode/secrets/`; the folder holds no secret.

Nothing else is required and nothing has to be installed: the cell talks to
BoxCompute's MCP server at `https://mcp.boxcompute.ai/mcp` itself. `/boxcompute`
with no argument shows the endpoint and whether a key is there, and
`(help :boxcompute)` is the model's guide to the tools.

## Use it

Ask for what you want; the model picks the tool:

| Tool | Use |
| --- | --- |
| `boxcompute_sandbox` | list the account's VMs, create one, start, stop or delete one |
| `boxcompute_run` | one shell command, up to 60 seconds, answered with its output |
| `boxcompute_process` | a long command out of the way: start, then get, output, cancel |
| `boxcompute_file` | read, list, write and edit files under `/workspace` |
| `boxcompute_preview` | a public https link to a port in the VM, for one hour |
| `boxcompute_tool` | any of the server's own tools by name, with its own arguments |

From the keyboard, `/boxcompute list` (or `get`, `create`, `start`, `stop`,
`delete`) runs that one call on the account's VMs.

A running VM spends the account's credit every minute. It stops by itself 30
minutes after its last command, file or preview call, and the model is told
to stop it when the work is done — `stop` keeps `/workspace`, `delete` does
not. A preview link works for one hour and anyone who has it can open it.

Everything happens in the user's BoxCompute VMs, never on their machine:
commands run under `/workspace` in the VM, and the only thing that leaves
this one is the key and the call.

### The six tools are the plugin's own

The original was a manifest, a skill and one `hermes mcp add` line pointing at
BoxCompute's hosted MCP server; Hermes discovered the server's tools and
offered them to the model. This cell keeps the same service, the same account
and the same tools, but speaks the MCP wire itself (JSON-RPC over streamable
HTTP, `initialize` and a session id, then `tools/call`), so it needs no MCP
configuration, no OAuth browser round and no second program.

`(boxcompute:tools)` lists the server's catalog as it is now, and
`(boxcompute:schema "sandbox_create")` one tool's exact arguments — the
authority when the server carries a tool or a member these declarations do
not. `boxcompute_tool` passes those arguments through unchanged.

## Not ported

- **The OAuth sign-in.** The plugin sent the user through a browser and let
  Hermes keep and refresh the token. A cell's section is one key, so an API
  key is the way in here; a key made with fewer scopes shows fewer tools.
- **Discovery at connect time.** The six tools declare their argument names,
  so a change in a hosted tool's schema reaches the model through
  `(boxcompute:tools)` rather than automatically.
- **The OAuth-over-SSH guidance and the `bxc` CLI note** from the skill: the
  first is Hermes's own login flow, the second a local CLI this cell replaced.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-boxcompute
```

Every request is a stubbed `nlk:http`, so nothing reaches BoxCompute and no
key is needed.

MIT licensed, as the original is.
