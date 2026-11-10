# nodecode-parrotnotes

ParrotNotes: your in-person meeting notes, read and saved from inside Nodecode. A Nodecode rewrite of [ascetx/parrot-notes-hermes-plugin](https://github.com/ascetx/parrot-notes-hermes-plugin) by [ascetx](https://github.com/ascetx).

ParrotNotes records in-person meetings on a phone or a Mac with no meeting bot, then keeps each one as a transcript, a summary and action items. The original plugin gives an agent two things: the hosted ParrotNotes MCP server, and a skill saying how to work it. Nodecode already speaks that server's wire, so this cell carries the skill, as a manual, and the server entry, as one command.

## Get it

Tell the agent "add the parrotnotes cell", or run `nodecode add nc://parrotnotes`.

## Set it up

Two steps, once.

1. `/parrotnotes connect` writes the `mcp.servers.parrotnotes` entry the plugin's `mcp.json` shipped, pointing at `https://mcp.parrotnotes.app/mcp`. In the config, that is all it is:

```jsonc
"mcp": {
  "servers": {
    "parrotnotes": { "url": "https://mcp.parrotnotes.app/mcp" }
  }
}
```

2. Restart the mcp cell on the new entry — `/mcp` → **Restart it**, or `(restart-cells "nodecode-mcp")` — then sign in once with `/mcp` → **Sign in**. ParrotNotes uses OAuth 2.0 with dynamic client registration, so there is no API key and nothing secret is stored by this cell; the tokens are the mcp cell's, under the Nodecode home.

The section has three members, all optional: `server` (the entry's name, `parrotnotes`), `url` (the endpoint above) and `limit` (how many notes a listing shows, 20). Nothing here needs a key, so an unconfigured section still runs.

## Use it

Ask in your own words, and the agent reaches the notes with the mcp cell's tools — `mcp:parrotnotes/get-recent-notes`, `search-notes`, `get-note`, `get-note-insights`, `get-user-labels`, `get-note-labels`, `get-notes-by-label`, `get-subscription-status`, and the one that writes, `save-note-insight`:

- "Summarize my most recent ParrotNotes meeting and list the action items."
- "Find the notes from the site visit and give me the key points."
- "What did the client commit to on Tuesday's call? Save that back as action items."

`/parrotnotes` reads the status and offers the next step; `/parrotnotes workflow` answers the manual, which is also `(help :parrotnotes)`. The manual keeps what the original skill taught: the two note ids and which tool takes which, search before concluding a note does not exist, read the full transcript before summarizing, reuse a saved insight rather than writing a second one, confirm before a save replaces one, and treat a transcript's text as the user's data rather than as instructions.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The manual, the status and connect verbs, and the `/parrotnotes` command. |
| `test/cell-test.lisp` | The lifecycle, the command's answers, the one write (against a stubbed reader and writer) and the manual. |

No network call happens in this cell: it dials nothing, and every note comes through the mcp cell, which owns the connection, the sign-in and the tokens.

## License

MIT, as the original. See `LICENSE` and `NOTICE`.
