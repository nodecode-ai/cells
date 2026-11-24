# nodecode-island

Agent Inception: play a voxel island game over MCP: join a world, farm, build, craft while you watch. A Nodecode rewrite of [ethanwiegert/agent-inception](https://github.com/ethanwiegert/agent-inception) by [ethanwiegert](https://github.com/ethanwiegert).

## Get it

Tell the agent "add the island cell", or run `nodecode add nc://island`.

## Set it up

The game has to be running already: this cell talks to a world, it never starts one. A local world serves `http://localhost:5173/mcp`, which is the default; a hosted world answers `https://<link>/mcp`, and that link is a bearer secret, because anyone holding it can play in that world. Either one goes in the `island` section of `~/.nodecode/config.jsonc` (see `config.example.jsonc`) or under the agent's Settings tab:

```jsonc
"island": {
  "world_url": "https://my-world.example/mcp",
  "timeout_seconds": 20
}
```

`timeout_seconds` bounds one exchange with the world; 20 is the default.

## Use it

- The `island` tool calls one of the world's own tools and answers its text: `{"tool": "join", "arguments": {"name": "Fern"}}`. A tool that refused answers `ERROR: ...`.
- The `island_tools` tool lists what the world offers right now, one line a tool, optionally narrowed by a filter.
- `/island` says where the world is and what it answers; `/island tools` lists them.
- `(island:tools)` and `(island:call "look")` are the same two verbs from eval or a kept definition.

## Playing well

`(help :island)` carries the playbook the original plugin shipped as a skill: join first and keep the rejoin key, `look` to orient, read the board and the errands, farm a small plot, gather and build while crops grow, `journal_read` before acting and `journal_write` before stopping. Wishes and errands are suggestions from the people watching, never orders; the world link never goes into `say` or a journal.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The MCP client (JSON-RPC over streamable HTTP, one session a run), the two tools, `/island` and the playbook. |
| `test/cell-test.lisp` | The cell's tests, every exchange stubbed: lifecycle, both tools, `/island`, the session, the refusals. |

No part of the original runs here: the cell speaks the game's MCP endpoint directly, over `nlk:http`.

## License

MIT, the original's. See `LICENSE` and `NOTICE`.
