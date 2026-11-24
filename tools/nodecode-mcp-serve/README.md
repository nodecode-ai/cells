# nodecode-mcp-serve

MCP Serve: this Nodecode as an MCP server, so other agents list, read and send your conversations. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

Any MCP client — Claude Code, Cursor, Codex, a harness of your own — gets
your Nodecode's conversations as six tools: list them, read one, read its
recent messages, poll what is new since a cursor, send into one, and see
which platforms they began on. Reads need nothing running beyond the
gateway already serving this session; a send goes through the same ingress
the shell uses and starts a turn in that session.

## Get it

Tell the agent "add the mcp-serve cell", or run `nodecode add nc://nodecode-mcp-serve`.

## Set it up

Nothing is required: the cell starts on its defaults and answers on
`/mcp-serve` of the gateway's own port, public to local clients. Its
section, in `~/.nodecode/config.jsonc`:

```jsonc
"mcp-serve": {
  "path": "/mcp-serve",   // the route on the gateway's port
  "public": true,          // a local client needs no operator token
  "read_only": false,      // true: messages_send refuses
  "max_messages": 50       // the most messages one read answers
}
```

`public: false` makes the client carry the operator token, which the
client config has no field for — leave it on unless the box has other
users. `read_only: true` is the setting to choose when the client is an
agent you do not trust to speak as you.

## Use it

Type `/mcp-serve` in the shell and it prints the line to paste into the
client, with the port this gateway answers on:

```json
{"mcpServers": {"nodecode": {"type": "http", "url": "http://127.0.0.1:PORT/mcp-serve"}}}
```

Restart the client, and it lists the tools:

| Tool | What it does |
|---|---|
| `conversations_list` | every conversation: session_id, title, platform, directory, last activity |
| `conversation_get` | one conversation's details |
| `messages_read` | a conversation's recent messages, oldest first, with the cursor |
| `events_poll` | messages logged after a cursor; called with no cursor it answers where to start |
| `messages_send` | send text into a conversation, through `nle:submit` |
| `channels_list` | the platforms the conversations began on, and how many each |

From Lisp the same things answer directly: `(mcp-serve:client-config)`,
`(mcp-serve:tool-names)`, `(help :mcp-serve)`.

Stop it with `/cells` (or `(stop-cell "nodecode-mcp-serve")`): the route
comes off and the client's next call fails to connect, as it would with any
stopped server. No session is left open and nothing is written.

MIT licensed, a rewrite of NousResearch's MCP Serve plugin; see NOTICE.
