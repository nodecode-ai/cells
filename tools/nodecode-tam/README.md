# nodecode-tam

TAM Memory: saves durable facts to a total-agent-memory store and recalls the memories nearest a query. A Nodecode rewrite of [vbcherepanov/hermes-tam-memory](https://github.com/vbcherepanov/hermes-tam-memory) by [vbcherepanov](https://github.com/vbcherepanov).

The original is a Hermes Agent memory provider. This cell gives Nodecode the same two tools the original gave its harness -- `tam_recall` and `tam_save` -- over the same MCP surface (the `memory_recall` and `memory_save` tools of a TAM server), plus `/tam` and the same two calls from eval. It talks to a TAM server over HTTP; it does not spawn the `tam` binary.

## Get it

Tell the agent "add the tam cell", or run `nodecode add nc://tam`.

## Set it up

You need a running TAM server and its MCP endpoint:

```
tam-team --root /srv/tam serve --host 127.0.0.1 --port 3737
```

Then set the `tam` section: `url` is that server's MCP endpoint (for example `http://127.0.0.1:3737/mcp/`). A team server also asks for a bearer token: put it in the environment variable `token_env` names, by default `TAM_API_TOKEN`. `project` is where a save that names no project lands; `recall_limit` is how many memories a recall that names none gets back; `timeout_seconds` bounds one exchange.

Set it in `/settings` or in the TUI's Settings tab. A section with no `url` refuses to start, and says so.

## Use it

Ask the agent to remember something, or to check what it already knows:

- `tam_recall` takes a query and answers the memories TAM ranks highest for it, best first, one line each.
- `tam_save` stores one fact, decision, solution, lesson or convention, with optional tags, importance and context. TAM's own quality gate may refuse it, and it may answer that the memory was already there.
- `/tam` asks the server what it is; `/tam QUERY` recalls.
- From eval: `(tam:recall "deploy steps" :limit 5)` and `(tam:save "staging runs on port 8080" :type "fact" :tags '("deploy"))`.

The store is shared: every other agent pointed at the same TAM -- a coding agent, another editor -- reads what this cell saves, which is the point of TAM. Where the memories live and who may read them is the server's business, not this cell's.

## Credit

The tools, their arguments and their two wire contracts are vbcherepanov's, from the original plugin; the Lisp is Nodecode's. MIT, as the original is: see LICENSE and NOTICE.
