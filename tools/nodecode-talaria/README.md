# nodecode-talaria

Talaria WebUI: reads the Hermes Agent API server's talaria-webui routes, giving Nodecode four tools over a server's sessions. A Nodecode rewrite of [liamsmith86/talaria-webui](https://github.com/liamsmith86/talaria-webui) by [liamsmith86](https://github.com/liamsmith86).

The plugin adds `/talaria/v1` routes to a Hermes Agent API server: cross-session history search, one reply's metadata, one session's context use, and the session changes a browser client needs. This cell is a client of those routes, so the same data reaches the model here.

## Get it

Tell the agent "add the talaria cell", or run `nodecode add nc://talaria`.

## Set it up

The cell needs a Hermes Agent API server that runs the talaria-webui plugin, and that server's API key.

```jsonc
{
  "talaria": {
    "url": "http://127.0.0.1:8080",   // the server's base URL, scheme included
    "key_env": "HERMES_API_KEY",     // the environment variable holding its key
    "timeout": 15
  }
}
```

The key is the server's own, set with `API_SERVER_KEY` on the server; the cell stores none of its own and sends it as a bearer token. A missing key, or a URL with no `http://` or `https://`, refuses at start and names what to fix. On the server, the plugin must also be enabled for the profile that serves the key: a profile it is not enabled for answers 404, which `talaria_capabilities` reports.

## Use it

Ask in words — "what did we decide about the kitchen wiring last week?" — or call the tools:

- `talaria_capabilities {full}` — the plugin's version, whether history search and session changes are on, how many commands it exposes, the agent name. Call it first when another tool says the plugin is not enabled.
- `talaria_search_history {query, offset}` — pages of twenty matches across every session the key can see: session id, title, role, snippet. Then read one session further.
- `talaria_reply_details {session_id, message_id}` — one reply's record: finish reason, model, provider, input and output tokens, whether it carried reasoning.
- `talaria_context_usage {session_id}` — how full a session's context window is: input tokens last sent, the model's maximum, when it was observed.

From eval a person can call the same functions: `(talaria:search-history ...)`, or `(help :talaria)` for the primer.

Every tool is a GET, so nothing here changes a session a person is working in. The tests stub the HTTP seam and the key environment: they run offline.

```sh
.github/test-cell.sh tools/nodecode-talaria
```
