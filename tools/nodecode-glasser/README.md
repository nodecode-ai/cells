# nodecode-glasser

Glasser: 1,000+ paid data endpoints behind one key, as seven tools. A Nodecode rewrite of [glasser-ai/hermes-plugin-glasser](https://github.com/glasser-ai/hermes-plugin-glasser) by [glasser-ai](https://github.com/glasser-ai).

[Glasser](https://glasser.ai) is a data broker: one key covers endpoints
from providers such as Apollo, People Data Labs, Hunter, Ahrefs, Semrush,
DataForSEO, Serper, Exa, ScrapeCreators and RentCast, and each call is
charged the endpoint's published price. The original is a manifest that
points a coding agent at Glasser's remote MCP server. This cell is that
server, reached over MCP's streamable HTTP with the cell's own client, so it
needs no Node bridge and no `npx`.

## Get it

Tell the agent "add the glasser cell", or run `nodecode add nc://glasser`.

## Set it up

Make a key at [app.glasser.ai/keys](https://app.glasser.ai/keys), then put it
in the `glasser` section of `config.jsonc` (or `/settings`):

```jsonc
{
  "glasser": {
    "api_key": "…",                                  // or leave it out and export GLASSER_API_KEY
    "endpoint": "https://api.glasser.ai/mcp",       // the public server, the default
    "timeout_seconds": 60                            // one call's ceiling, a run included
  }
}
```

`api_key_env` names an environment variable instead, and `GLASSER_API_KEY` is
read either way, so a key exported in a shell needs no section at all. The
key can be revoked at app.glasser.ai/keys; a section with no key refuses at
start and says where to make one. `/glasser` answers which key is in use and
against which endpoint.

## Use it

The seven tools keep the original's names and arguments, under the cell's
prefix:

| Tool | Purpose | Costs |
| --- | --- | --- |
| `glasser_search` | the endpoints this key may run, filtered by what you describe | free |
| `glasser_inspect` | one endpoint's input schema, exact price with every charge clause, run mode | free |
| `glasser_run` | run an endpoint and charge its published price per call | per call |
| `glasser_runs_get` | one run by id: status, output, the provider's response, the charge | free |
| `glasser_runs_list` | this workspace's runs, newest first | free |
| `glasser_runs_stop` | stop an in-flight run; before dispatch it is free | free |
| `glasser_balance` | balance, held and available; the probe that says the key works | free |

Every run carries an idempotency key: a retry with the same key returns the
first run instead of paying twice, and a new key is a new charge. When the
model leaves the key out, the cell makes a UUID for it; a caller that means
to retry passes its own.

Through `eval`, the same seven are one verb:

```lisp
(glasser:call "balance")
(glasser:call "runs_list" (nlk:json-object "limit" 5))
```

While the cell runs, `(help :glasser)` answers the working agreement the
original ships as a skill — search, then inspect, then run; name the price
before a speculative, looped or bulk run; poll a QUEUED or RUNNING run;
read `charge_usd` on every run; treat `insufficient_balance` and
`rate_limited` as answers, not as errors to retry blind.

## Not ported

- **The browser sign-in.** The original's bridge, `mcp-remote`, opens
  glasser.ai in a browser and caches the token it gets. A cell takes a key
  from its section instead — the same key the sign-in resolves to, listed at
  app.glasser.ai/keys — so nothing here holds an OAuth token or refreshes one.
- **The Node bridge itself.** `mcp.json` runs `npx -y mcp-remote@0.14.3`;
  this cell speaks the same MCP endpoint over HTTP directly.
- **The skill as a skill.** Its guidance is `(help :glasser)` here, one
  manual topic rather than a file loaded through `skills_list`.

## Gaps

- The cell declares each tool's arguments as the original's README and skill
  describe them (`query`, `endpoint`, `input`, `idempotency_key`, `run_id`,
  `limit`); it does not read the server's own `tools/list` schemas before
  registering, so a server-side rename would show as the server's own
  refusal rather than as a repaired argument list.
- Every call opens its own MCP session, initializes it, and leaves it for
  the server to expire: a cell call is one exchange, and no state is kept
  across calls.
- The release's MCP client already reaches this same server by config
  (`mcp.servers.glasser`), with its own OAuth sign-in. This cell is for
  installing Glasser as one thing, with its tools named, its key in one
  section, and its spending rules on the manual.

## Test

```sh
.github/test-cell.sh tools/nodecode-glasser
```

Every HTTP exchange is a stubbed `nlk:http`, so nothing reaches Glasser or
the network: the lifecycle, the key's two ways in, `/glasser`, and each
answer shape — a balance, a run with a generated key, a refusal the server
reports, a refused key. A green run ends `12 glasser tests, 0 failed`.

MIT licensed. Glasser's own guidance and manifest were written by glasser-ai;
see [NOTICE](NOTICE).
