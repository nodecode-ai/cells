# nodecode-cospark

Cospark: research ad references and make images, video ads and voiceovers through its hosted MCP server. A Nodecode rewrite of [corift/cospark-ai](https://github.com/corift/cospark-ai) by [corift](https://github.com/corift).

## Get it

Tell the agent "add the cospark cell", or run `nodecode add nc://cospark`.

## Set it up

Cospark is a hosted service: it makes ad creative on your own Cospark account, and the cell talks to Cospark's own MCP server at `https://api.cospark.so/mcp`. One of these two gets it a sign-in:

1. **A sign-in you already have.** Add the server in `/mcp` as `{"url": "https://api.cospark.so/mcp"}` and finish its browser login. The shipped mcp cell keeps that token and this cell reuses it; nothing else is needed.
2. **A token.** Paste one into the `cospark` section's `token` member in `/settings`, name an environment variable in `token_env`, or write it into a file and name that file in `token_file`.

`server_url` defaults to `https://api.cospark.so/mcp`. `/cospark login` says, in words, where the sign-in in use came from, and never prints it.

## Use it

| Tool | Use |
| --- | --- |
| `cospark_docs` | Cospark's maintained workflow guides, read from the server: no path for the index, one path for one guide |
| `cospark_tools` | every tool the server offers: its name, what it does, whether it is read-only, and the arguments it takes |
| `cospark_call` | one of them by name, with that tool's own arguments object |

The first call is `cospark_docs` with no path: the server's guides are the workflow (research, prompting, generation, inspection, project files), and they change without this cell moving. Then `cospark_tools` names the tools of the moment and the arguments each takes, and `cospark_call` runs one. Nothing about the server's tools is restated here.

Generation, media inspection and social research can spend your Cospark credits, so `cospark_call` refuses a tool the server does not mark read-only until `confirmed` is `true`, and that flag is one person's yes to one call. Reading documentation never needs one. The rule is the skill's, and it is enforced here rather than asked for.

From the keyboard, `/cospark` lists the tools the server offers and says where the sign-in came from, and `/cospark reference` lists the ones whose name or description says reference. The model reads the workflow with `(help :cospark)`.

The skill the original shipped is that manual topic, so its one line rides every request and the whole of it arrives when the model asks: read the guides first and use the live schemas; a request for research, a prompt or a plan does not authorize paid generation; keep the chosen model, the exact dialogue, the supplied assets and the scope; preserve run ids and poll rather than resubmitting; inspect the output before claiming its product details, speech or timing are right; share the links and save project files only when asked.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The skill as the manual topic, the section, the MCP wire over `nlk:http`, the three tools and the `/cospark` command. |
| `package.lisp` | The package, and the peripheral declaration. |
| `config.example.jsonc` | The section this cell reads, commented; documentation, not config. |

## Not ported

- **The manifests.** `plugin.json`, `.mcp.json`, the Codex manifest and the marketplace entry said where the server is and what the skill is called; a cell says the server once, in its section, and registers its own tools and manual.
- **The OAuth browser flow.** The shipped mcp cell owns it; this cell reuses the token that sign-in keeps, or takes a pasted one.
- **A tool of its own for each of the server's tools.** The repository carries no schema for them, only the workflow around them, so the three tools keep the server's own names and arguments rather than restating a list that would go stale.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-cospark
```

Every request is a stubbed `nlk:http`, so nothing reaches Cospark, no token is needed and no credit is spent.

MIT licensed, as the original is. See `NOTICE`.
