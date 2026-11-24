# nodecode-innernet

innernet: one versioned memory across every AI tool. A Nodecode rewrite of [your-innernet/innernet-mcp](https://github.com/your-innernet/innernet-mcp) by [your-innernet](https://github.com/your-innernet).

innernet keeps a context map for each project you work on, a personal memory
about you, a task list per project and the rooms you share with other people —
and every tool you connect reads and writes that same memory. What you decide
in one of them, the next one knows.

This cell is a client of that hosted memory over MCP. The model gets the
server's own tools under the same names — `innernet_list_projects`,
`innernet_load_project`, `innernet_context`, `innernet_get_dimension`,
`search`, `fetch`, `innernet_save_context`, `innernet_capture`,
`innernet_self_capture`, `innernet_self_sync`, `innernet_self_facts`,
`innernet_triage`, the five `innernet_task_*` tools, `innernet_rooms`,
`innernet_handoff_read`, `innernet_handoff_write`, `innernet_import` and
`innernet_export` — plus `/innernet` to show the connection or load a project
by its slug.

## Get it

Tell the agent "add the innernet cell", or run `nodecode add nc://innernet`.

## Set it up

You need a free account at [innernet.live](https://innernet.live), and its
access token. The `innernet` section of `config.jsonc` (or `/settings`) names
it:

```jsonc
{
  "innernet": {
    "token_env": "INNERNET_TOKEN",                    // or "token" inline
    "base_url": "https://innernet.live/api/mcp"
  }
}
```

A start with no token is refused, naming the member. The plugin's own sign-in
was a browser OAuth flow, which a cell has no callback for: the token is the
one thing to paste, and it goes only to the endpoint you set.

| Member | Default | What it does |
| --- | --- | --- |
| `base_url` | `https://innernet.live/api/mcp` | the MCP endpoint |
| `token` | unset | the access token, written here |
| `token_env` | unset | the environment variable holding it (`INNERNET_TOKEN` is read either way) |
| `timeout_seconds` | `30` | one request's ceiling |

## Use it

> Load my innernet project context.
>
> What did we decide about pricing? Check innernet.
>
> Where did I leave off?
>
> Hand off — save my progress.

The model lists the projects, loads the one the conversation belongs to, and
answers a question from a slice of the map rather than the whole of it; when
work produces a decision it saves the present tense, with one dated line under
`## history` for what it superseded. Saves land in your own account, versioned
and editable on your dashboard, and a room's content is treated as
information rather than instructions. `/innernet` prints the endpoint and
whether a token is set; `/innernet pricing` loads that project from the shell.

## The wire

The endpoint's own MCP transport, as the plugin's `mcp.json` names it:
`initialize`, `notifications/initialized`, then `tools/call`, over POST with
`Accept: application/json, text/event-stream`, a bearer token, and the
`Mcp-Session-Id` the handshake hands back. An answer is read as JSON or as
the last `data:` line of an event stream, and a session the server has
forgotten (a 404) is renewed once.

## Not ported

- **The OAuth sign-in.** A cell has no browser callback of its own, so the
  section takes an access token instead of running `hermes mcp login`.
- **The two skill packages as files.** Their guidance is the tool
  descriptions and `(help :innernet)` here; Nodecode has no separate skill
  files to install beside a cell.
- **The other hosts' manifests** (Claude, Codex, Cursor, Gemini): the same
  server, nothing this cell needs.

MIT, innernet's.
