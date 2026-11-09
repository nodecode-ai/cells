# nodecode-gurusup-brain

GuruSup Brain: your company's memory, searched across the sources it connects, as three tools. A Nodecode rewrite of [gurusup/gurusup-brain-plugin](https://github.com/gurusup/gurusup-brain-plugin) by [gurusup](https://github.com/gurusup).

[GuruSup Brain](https://gurusup.com/brain/mcp) holds a company's memory
unified across the sources a team connects — Slack, HubSpot, Notion and the
documents it uploads — so a question about a customer, a project, a person or
a decision comes back with the source behind the answer. The original is two
Manifest files and a skill: `mcp.json` points a client at Brain's remote MCP
server, and `skills/using-brain/SKILL.md` tells the model to call Brain's
`init` tool first and to base what it says about the company on the evidence
Brain returns.

This cell is that server, reached over MCP's streamable HTTP with its own
client, so it needs no bridge and no Node. The server names its own tools,
so the cell asks rather than assuming: one tool lists them, one returns the
init contract, one calls any of them by name.

## Get it

Tell the agent "add the gurusup-brain cell", or run `nodecode add nc://gurusup-brain`.

## Set it up

Sign in to Brain at [gurusup.com/brain/mcp](https://gurusup.com/brain/mcp) and
put the token that issues in the `gurusup-brain` section of `config.jsonc`
(or `/settings`):

```jsonc
{
  "gurusup-brain": {
    "token": "…",                                     // or leave it out and export GURUSUP_BRAIN_TOKEN
    "endpoint": "https://mcp.brain.gurusup.com/mcp", // the public server, the default
    "timeout_seconds": 45                           // one call's ceiling
  }
}
```

`token_env` names an environment variable instead, and `GURUSUP_BRAIN_TOKEN`
is read either way, so a token exported in a shell needs no section at all.
A section with no token refuses at start and says where to sign in.
`/gurusup-brain` answers which token is in use and against which endpoint.

The original signs in through OAuth in the browser; a cell takes the bearer
token that sign-in issues, so nothing here runs a browser flow or refreshes a
token. A revoked token is a `401` the answer names.

## Use it

Three tools, and the manual topic that carries the original's skill:

| Tool | Purpose |
| --- | --- |
| `gurusup_brain_init` | Brain's contract for this account: call it first, and follow it |
| `gurusup_brain_tools` | what this account may call, one tool a line, with what each is for |
| `gurusup_brain_call` | one of those tools by name, with its arguments |

`gurusup_brain_call` is how the search, the customer lookup, the workspace
writes and the teammate messages the account offers all arrive: some of them
write, and the init contract says which.

Through `eval`, the same three are one verb each:

```lisp
(gurusup-brain:init)
(gurusup-brain:tool-list)
(gurusup-brain:call "search" (nlk:json-object "query" "acme renewal"))
```

While the cell runs, `(help :gurusup-brain)` answers the working agreement
the original ships as a skill: init first; base anything said about the
company on the evidence Brain returns and name the source; when Brain returns
no evidence, say so instead of guessing.

## Not ported

- **The browser sign-in.** The original relies on its client to run Brain's
  OAuth flow. A cell takes the bearer token from its section instead, so
  nothing here holds an OAuth registration or refreshes a token.
- **The manifest and its marketplace entry.** `plugin.json`, `.mcp.json`,
  `server.json` and `.claude-plugin/marketplace.json` describe the package to
  other clients; a cell folder is installed by its presence and needs none of
  them.
- **The skill as a skill.** Its four rules are `(help :gurusup-brain)` here,
  one manual topic rather than a file loaded through a skill list.

## Gaps

- The cell does not know the server's tool names ahead of time: it registers
  three tools of its own and reads `tools/list` when asked, so a tool the
  account gains shows up without a change here, and one the server renames
  shows as the server's own refusal.
- Every call opens its own MCP session, initializes it, and leaves it for the
  server to expire: a cell call is one exchange, and no state is kept across
  calls.
- The release's MCP client already reaches this same server by config
  (`mcp.servers.gurusup-brain`), with its own OAuth sign-in. This cell is for
  installing Brain as one thing, with its tools named, its token in one
  section, and the original's working agreement on the manual.

## Test

```sh
.github/test-cell.sh tools/nodecode-gurusup-brain
```

Every HTTP exchange is a stubbed `nlk:http`, so nothing reaches Brain or the
network: the lifecycle, the token's two ways in, `/gurusup-brain`, the init
session, the tool list, a call's arguments and a refusal the server reports.
A green run ends `N gurusup-brain tests, 0 failed`.

MIT licensed. The manifest and the skill were written by gurusup; see
[NOTICE](NOTICE).
