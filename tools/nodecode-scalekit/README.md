# nodecode-scalekit

Scalekit connectors: approve a connection, call one of its tools, or call the connector's own API, as four tools the model calls. A Nodecode rewrite of [scalekit-inc/hermes-tools](https://github.com/scalekit-inc/hermes-tools) by [scalekit-inc](https://github.com/scalekit-inc).

## Get it

Tell the agent "add the scalekit cell", or run `nodecode add nc://scalekit`.

## Set it up

The cell talks to your own Scalekit environment with the three values the
Scalekit dashboard shows under Developers, Settings, API Credentials:
a client id (`skc_...`), a client secret, and the environment's address
(`https://your-env.scalekit.dev`). Either export them under the names the
original plugin read:

```sh
export SCALEKIT_CLIENT_ID=skc_...
export SCALEKIT_CLIENT_SECRET=...
export SCALEKIT_ENVIRONMENT_URL=https://your-env.scalekit.dev
```

or set them in `/settings` (or the page's Settings tab) under `scalekit`.
A secret pasted into the panel is kept in a 0600 file under
`~/.nodecode/secrets/`; the folder holds no secret.

`identifier` names the person these connections act for. Left empty, the
cell generates one (`usr_...`) once and keeps it under the Nodecode home, so
every run acts as the same person.

`/scalekit` with no argument lists the connections and where they live, and
`(help :scalekit)` is the model's guide to the tools.

## Use it

Ask for what you want; the model picks the tool:

| Tool | Use |
| --- | --- |
| `scalekit_list_connections` | the connection names this environment holds, with type and status |
| `scalekit_authorize` | approve one connection: ACTIVE, or the link that approves it |
| `scalekit_execute_tool` | call one of that connection's tools with its own arguments |
| `scalekit_proxy` | the connector's API for what no named tool covers |

From the keyboard, `/scalekit` lists the connections and
`/scalekit gmail-connect` answers that connection's approval link.

A connection has to exist, and be finished, in the Scalekit dashboard under
AgentKit, Connections before the cell can use it; the tools say so when one
is missing rather than retrying. Provider tokens (Gmail, Slack, GitHub)
stay in Scalekit: they never come back in a tool result.

### The four tools are the plugin's four tools

The original registered `scalekit_authorize`, `scalekit_execute_tool`,
`scalekit_list_connections` and `scalekit_proxy` against Hermes and moved
its calls to Scalekit's API through a small adapter. This cell calls the same
endpoints (the client_credentials token, the connections and
connected-accounts endpoints, `execute_tool`, and `/proxy`), so it needs no
Hermes runtime and no Python.

## Not ported

- **Hermes's plugin-data folder.** The original kept its generated person id
  under the Hermes home; this cell keeps one under the Nodecode home, and the
  section's `identifier` overrides it.
- **The JSON envelope around every answer.** The original returned its
  handler's JSON string; these tools answer the text a model reads, and the
  token-bearing keys are dropped the same way.
- **`missing_credential_message` as a tool answer.** A missing credential
  refuses the start instead, which is one standing notice under the cell's
  name rather than the same sentence from every tool.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-scalekit
```

Every request is a stubbed `nlk:http`, so nothing reaches Scalekit and no
credential is needed.

MIT licensed, as the original is.
