# nodecode-spectator

Spectator: a live, privacy-masked page of one session that invited guests open in a browser, and the suggestions they leave. A Nodecode rewrite of [Sahil-SS9/spectator-mode](https://github.com/Sahil-SS9/spectator-mode) by [Sahil-SS9](https://github.com/Sahil-SS9).

## Get it

Tell the agent "add the spectator cell", or run `nodecode add nc://spectator`.

## Set it up

The page is served on the gateway's own port, which is loopback. To let a guest open it, put something in front of that port — a tunnel, a reverse proxy — and set its public address:

```jsonc
{
  "spectator": {
    "public_url": "https://share.example.org",
    "title": "Spectator",
    "invite_minutes": 120,
    "max_events": 200,
    "mask": true
  }
}
```

`public_url` is the only required setting: until it is set, `/spectator share` refuses and says so. The rest have defaults. Nothing else is needed — no account, no key, no second server.

## Use it

In a session's shell:

- `/spectator` — the invites open on this session and what waits behind them.
- `/spectator share [minutes]` — mint an invite and answer the address; "share this session" to the model does the same through the `spectator_share` tool.
- `/spectator suggestions` — what guests left, oldest first, each by id.
- `/spectator approve <id>` — put one into this session as a prompt beginning `[spectator]`, and answer what was sent. Nothing is injected without this.
- `/spectator reject <id>`, `/spectator close`.

The page polls every two seconds and shows, from the moment the invite was minted: a turn's start and end, each tool call by its name, and each assistant message's text. The prompt you typed is never quoted — only "a turn started".

### What a guest may read

One mask runs over every line before it leaves: an absolute path (POSIX, `~/`, or a drive letter) becomes `~/...`, a `NAME=value` assignment becomes `NAME=***`, and a long mixed token becomes `***`. A line is clipped to 400 characters. What travels is a summary of the session, never a transcript, and nothing is stored past the process: an invite dies with it, and no file is written.

The token in the address is the whole credential, so treat the link as one; `/spectator close` revokes every open invite at once.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The declaration: the section, the `spectator_share` tool, `/spectator`, `(help :spectator)`, the `/spectator/` route and the entry. |
| `test/` | Offline tests: a real store, a stub for the one seam that would start a turn. |

## License

MIT. The original is Copyright (c) 2026 Sahil Saghir: see `LICENSE` and `NOTICE`.
