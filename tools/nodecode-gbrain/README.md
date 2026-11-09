# nodecode-gbrain

GBrain Retrieval Reflex: the top pages of a GBrain brain, in front of the model before it answers. A Nodecode rewrite of [aean0x/hermes-gbrain-retrieval-reflex](https://github.com/aean0x/hermes-gbrain-retrieval-reflex) by [aean0x](https://github.com/aean0x).

A GBrain server holds your own pages — people, projects, notes — behind an
HTTP MCP endpoint. This cell is the client half: it asks that endpoint two
questions, `volunteer_context` (which named entities does this conversation
mention?) and `query` (what pages does this subject touch?), merges the two
answers, de-duplicates them, ranks them, and puts the top few in front of
the model as a short block: title, slug, strength, one-line preview.

The model gets them two ways: as `gbrain_recall`, a tool it calls when a
turn turns on a name or an older decision, and — unless you turn `ambient`
off — pushed into the first request of every turn, so the model reads them
without having decided to look. A turn that is only "thanks" pays for no
round trip, and a brain that is down, slow or unconfigured costs a turn
nothing: the hook stays silent.

## Get it

Tell the agent "add the gbrain cell", or run `nodecode add nc://gbrain`.

## Set it up

The cell talks to a GBrain server you run yourself (`gbrain serve --http`),
so the section has one thing to get right — the address and the token:

```jsonc
{
  "gbrain": {
    "url": "http://127.0.0.1:3131/mcp",
    "token_env": "GBRAIN_TOKEN"          // or "token" inline, or "token_file"
  }
}
```

`url` already defaults to the local server. The token is whichever of these
you fill in: `token` (an inline secret), `token_env` (the name of an
environment variable holding it, the default name is `GBRAIN_TOKEN`), or
`token_file` (a file holding just the token — the one `gbrain auth` mints
under its state directory, for instance `~/.gbrain/hermes-mcp.token`).
The rest of the section has working defaults; `/settings` shows every
member, and `config.example.jsonc` in this folder lists them.

## Use it

Ask the model to recall something — "what do we know about Ada?" — and it
calls `gbrain_recall`. Or ask yourself, from the shell:

```
/gbrain the migration decision
```

and from Lisp, through eval:

```lisp
(gbrain:recall "the migration decision")
```

That answers one string: the ranking, strongest first, an entity hit above a
topical one. The preview is cut short on purpose — a page is a pointer, so
the model reads the page itself (a GBrain page read over MCP) before relying
on a detail. Every hit names its slug so it can be read.

`(help :gbrain)` carries the same story to the model while the cell runs.

## What it does not do

The rewrite speaks the HTTP MCP endpoint only. The original's optional
`.gbrain-resolve.sock` fast path, its local audit marker and its
`$HERMES_HOME/.env` scraping are gone; the token file setting covers the
last one. `NOTICE` says what that leaves out and why.

## Licence

MIT, as the original. See `LICENSE`.
