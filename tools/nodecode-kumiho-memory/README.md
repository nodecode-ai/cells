# nodecode-kumiho-memory

Kumiho Memory: recall and save decisions, preferences and facts across chats in a hosted Kumiho Cloud workspace. A Nodecode rewrite of [KumihoIO/kumiho-plugins](https://github.com/KumihoIO/kumiho-plugins) by [KumihoIO](https://github.com/KumihoIO).

## Get it

Tell the agent "add the kumiho memory cell", or run `nodecode add nc://kumiho-memory`.

## Set it up

The cell needs a Kumiho Cloud workspace and an API key for it: sign in at <https://kumiho.io>, make a key in the dashboard's API keys, then either write it in `~/.nodecode/config.jsonc` or name the variable that holds it.

```jsonc
"kumiho": {
  "api_key_env": "KUMIHO_API_KEY",
  "mcp_url": "https://mcp.kumiho.cloud/mcp",
  "timeout_seconds": 30
}
```

`KUMIHO_API_KEY` in the environment is read even when the section names no variable. The key goes only to the endpoint `mcp_url` names, in the `x-api-key` header; nothing else is sent anywhere.

## Use it

Ask what it remembers, or tell it to remember something:

> What did we decide about the weekly planning meeting, and why?

> Remember this decision: the planning meeting is on Monday so everyone can attend.

The model calls one of seven tools, and `(help :kumiho)` has the protocol it follows:

| Tool | What it does |
| --- | --- |
| `kumiho_recall` | What the workspace already holds on a topic: earlier decisions, preferences and facts, ranked by relevance. |
| `kumiho_latest` | The newest saved memories, newest first. |
| `kumiho_remember` | Save one distilled memory: a decision, a preference, a durable fact, or a correction of one (pass its kref as `revises`). |
| `kumiho_forget` | Retire one memory by its kref so recall stops returning it. Reversible, not erasure. |
| `kumiho_spaces` | The spaces the workspace holds, so a save can name one that exists. |
| `kumiho_one_memory` | One memory in full, by its kref and tag (`latest` by default), when a recall returned only a summary. |
| `kumiho_summary` | Fold a summary the user asked for into long-term memory and clear this conversation's buffer. |

`/kumiho QUERY` is the operator's own recall; `/kumiho` alone says where the cell points.

## How it talks to Kumiho

The cell is a small MCP client: `initialize`, `notifications/initialized`, then `tools/call` over streamable HTTP against `mcp_url`, answering either a JSON body or an SSE stream. It calls the same hosted tools the original plugin's skills called -- `kumiho_memory_engage`, `kumiho_memory_retrieve`, `kumiho_memory_reflect`, `kumiho_deprecate_item`, `kumiho_get_spaces`, `kumiho_get_revision_by_tag`, `kumiho_memory_consolidate` -- and hands their text to the model unchanged.

The buffer tools (`remember`, `summary`) follow the original's session rule: the first call carries no `session_id`, a `session_required` answer's id is kept, and the same call is retried once with it, so a save is never duplicated. A lost transport session is re-handshaked once.

## What is left out

The original's six separate skills are one primer here; OAuth sign-in is replaced by the API key, since Nodecode has no browser sign-in for an MCP server to borrow; the chat-buffer pair, space creation and space profiling are not exposed as tools; and the backfill helper for Codex, Claude Code and ChatGPT exports is not ported. See `NOTICE`.

## Test

Run `.github/test-cell.sh tools/nodecode-kumiho-memory` from the cells repository. Every request is stubbed, so the tests need no key and no network.

## License

MIT, Copyright (c) 2025 Kumiho Inc.: see `LICENSE` and `NOTICE`.
