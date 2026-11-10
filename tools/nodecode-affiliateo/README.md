# nodecode-affiliateo

Affiliateo: run your business from Nodecode — affiliates and payouts, products and sales pages, revenue and customers, invoices, leads and booked calls, paid ads, funnels, courses, community, support and Live Chat. A Nodecode rewrite of [affiliateo/agent](https://github.com/affiliateo/agent) by [affiliateo](https://github.com/affiliateo).

The original is a skill plus a connection to Affiliateo's hosted MCP server. This cell is the same two things: the server's tools, reached over MCP's streamable HTTP, and the skill's working logic as the manual.

## Get it

Tell the agent "add the affiliateo cell", or run `nodecode add nc://affiliateo`.

## Set it up

You need an [Affiliateo](https://affiliateo.com) account and an API key:

1. Open any business dashboard, then the **API** tab.
2. Create a key. It starts with `afk_`. A key you want to pay affiliates with needs its **Payouts** switch on; payouts stay within that key's daily limit.
3. Name the key, never write it into the config: `api_key_env` holds the name of the environment variable that carries it, `api_key_file` a file only you can read. Set exactly one.

```jsonc
"affiliateo": {
  "api_key_env": "AFFILIATEO_API_KEY",   // or a file: "api_key_file": "~/.config/affiliateo-key"
  "timeout": 30
}
```

| Setting | What it does |
| --- | --- |
| `api_key_env` | The environment variable holding the key (export it in the shell that starts Nodecode). |
| `api_key_file` | A file holding the key on its own first line. |
| `url` | The MCP server's address; default `https://affiliateo.com/api/mcp`. It must be https on `affiliateo.com`, because the key travels nowhere else. |
| `timeout` | How long one request may take, in seconds; default 30. |

The original also signs in with a browser (OAuth) instead of a key, which the Hermes package does not use. This cell takes a key: it is the one that can also pay people. To sign in instead, add Affiliateo's server to the `mcp` cell (`hermes mcp add` has no counterpart, but `mcp.servers` takes a URL) and use `(mcp:call "affiliateo" ...)`; the manual here is what teaches the tools either way.

## Use it

Ask in plain words: "how much revenue did we make this week?", "who are my top five affiliates, and what do I owe them?", "which Meta placements are wasting my budget?", "who booked a call today?", "add a lesson called Getting started to my course".

The model reaches every one of the server's tools — over 120 of them — through two verbs:

| Tool | What it does |
| --- | --- |
| `affiliateo_tools` | The server's catalog, one line a tool. `filter` narrows it by a word; `tool` reads one tool in full, with its arguments as a JSON Schema. |
| `affiliateo_call` | One server tool with its arguments: `{"tool": "list_affiliates", "arguments": {"slug": "acme"}}`. |

A person can call the same two from eval:

```lisp
(affiliateo:list-tools "ad")
(affiliateo:call-tool "list_businesses")
(affiliateo:call-tool "get_ad_stats" :slug "acme")
```

`(help :affiliateo)` is the model's manual, kept while the cell runs: which tool does what, which writes reach a person or money, and how to read cents and micros. The plugin's three deeper guides are on the manual too, unchanged: `(help :affiliateo-ads)`, `(help :affiliateo-money)`, `(help :affiliateo-developers)`.

`/affiliateo` answers what the cell talks to; `/affiliateo tools [filter]` lists what the server offers.

### What the guards are

- **One host.** Every request goes to `https://affiliateo.com/api/mcp` (or another https address on `affiliateo.com`); the key rides an `Authorization: Bearer` header and never a URL or an answer.
- **The server's words, never the model's.** A tool's answer is returned as it came; a call that answers `isError` is a failed call, and the server's own message is the answer.
- **A long answer is clipped** past 24,000 characters, and says so, rather than filling the context.
- **No retry of a payment.** The manual sends the model back to the operator before every write that reaches a person or money, and names the ones that cannot be undone.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The MCP client (initialize, tools/list, tools/call), the two tools, the `/affiliateo` command and the manual. |
| `contracts.lisp` | The two verbs' arguments, one declaration each: the schema the model is shown and the body that runs are one shape. |
| `skill/` | The plugin's three deeper guides — `ads.md`, `money.md`, `developers.md` — unchanged, on the manual as `:affiliateo-ads`, `:affiliateo-money` and `:affiliateo-developers`. |

## What the rewrite leaves out

- **A tool per verb.** The original loads all of the server's tools into the client; this cell keeps a fixed two and reads the server's catalog when the model asks, so a cell's contribution to every request stays one manual line and two tool schemas.
- **OAuth sign-in.** The Hermes package takes an API key; so does this cell.
- **Writing the skill back.** The skill's prose is the manual, not a `define-skill` the index ranks.

## License

MIT. The working logic and the guides are affiliateo's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
