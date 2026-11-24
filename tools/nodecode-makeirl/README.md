# nodecode-makeirl

MakeIRL: ERC, DRC, schematic-to-PCB parity, BOM and export checks on a KiCad 9
board before manufacturing, answered as BLOCKED, REVIEW or READY. A Nodecode
rewrite of [MelMayssonOwen/hermes-plugins](https://github.com/MelMayssonOwen/hermes-plugins)
by [MelMayssonOwen](https://github.com/MelMayssonOwen).

MakeIRL runs the checks on its own server; this cell is a thin client of the
MCP server it publishes, so a model can upload a release, start the run, read
the findings and explain them without leaving the conversation. The
release-gate workflow the original bundled as a skill is `(help :makeirl)`.

## Get it

Tell the agent "add the makeirl cell", or run `nodecode add nc://nodecode-makeirl`.

## Set it up

Sign in at makeirl.com, then give the cell the account's access token — in
`/settings`, or in `~/.nodecode/config.jsonc`:

| Member | What it is |
| --- | --- |
| `access_token` | the token, pasted in (Nodecode keeps it in a 0600 file under the home) |
| `token_env` | or the environment variable holding it, e.g. `MAKEIRL_TOKEN` |
| `token_file` | or a file holding it, 0600 |
| `endpoint` | MakeIRL's MCP address, default `https://makeirl.com/api/mcp` |
| `timeout_seconds` | how long one call may wait (default 120; a run is polled, never waited on) |

One of the three token members. A cell with none still loads — it says what
is missing when a tool is called, and the cells report says what the section
still needs. After setting it, `(restart-cells "nodecode-makeirl")`.

## Use it

Two tools, because MakeIRL's own tool list comes from the server at run time:

- `makeirl_tools` — what the server offers right now, with the arguments each
  tool takes and whether it is read-only. Call it before anything else.
- `makeirl_call` — one of those tools by name, its arguments in the
  `arguments` object.

From the operator's side, `/makeirl` lists the tools, and
`/makeirl release_status {"runId": "…"}` calls one. Through eval:
`(makeirl:server-tools)` and
`(makeirl:call "release_create_upload" (("product" . "blinker")))`.

The usual order: upload the KiCad project files (flat names, no
subdirectories), `release_start`, poll `release_status`,
`release_list_findings` and group the findings by severity. Fixing the project
is the person's call, and a new report needs a new upload.

## What it sends where

Every call goes to `makeirl.com`, with the token in the `Authorization`
header, on the endpoint the section names. Uploading a release sends that
project's KiCad files to MakeIRL. Checkout tools answer a payment link, and
the payment happens in the browser, not here. The tool list is the server's,
so it can change without this folder changing.

A report checks rules; it does not prove that a circuit works, and it covers
no firmware, RF, EMC, thermal behaviour or a manufacturer's own DFM review.
