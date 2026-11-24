# nodecode-hermesmade

Hermesmade Bots: the community bot shelf at hermesmade.lol — preview a bot, add it, share your own. A Nodecode rewrite of [Noshkoto/hermesmade](https://github.com/Noshkoto/hermesmade) by [Noshkoto](https://github.com/Noshkoto).

A bot on the shelf is a small package: a `SOUL.md` personality, an optional
`config.yaml`, the author's own `skills/`, and the schedules it asks for. This
cell is how a session reads one, adds one, and puts one of its own up.

## Get it

Tell the agent "add the hermesmade cell", or run `nodecode add nc://hermesmade`.

## Set it up

Nothing is required to look or add. The section is `hermesmade`:

| member | what it is |
| --- | --- |
| `site` | the shelf's address; `https://hermesmade.lol` unless you run your own |
| `bots_dir` | where added bots land; `bots/` under the Nodecode home unless this names a folder |
| `handle` | the `@handle` a shared listing is credited to |
| `timeout_seconds` | one request's ceiling, 45 by default |
| `max_bytes` | how large a downloaded bot may be, 2 MB by default |

Set `handle` before sharing, or pass `@you` on the command.

## Use it

From a conversation:

```
hermesmade:preview "7"                       ; what it is, before anything is installed
hermesmade:add "https://hermesmade.lol/bots/7-chef"
hermesmade:share "~/my-bot" :title "Chef" :tagline "Plans your week of dinners."
hermesmade:mine
```

or from the operator's side, `/hermesmade preview 7`, `/hermesmade add <id>`,
`/hermesmade share <folder> [title] [@you]`, `/hermesmade mine`,
`/hermesmade unshare <name>`. The model has the same three acts as tools:
`hermesmade_preview`, `hermesmade_add`, `hermesmade_share`.

**Adding.** The listing's `sha256` is checked before anything is unpacked, and
a package may hold only `SOUL.md`, `config.yaml`, `distribution.yaml`,
`hermesmade-bot.json`, `skills/` and `cron/`: one other file, an absolute path
or a `..` refuses the whole install. Nothing in a bot runs by itself — its
skills are Markdown and its schedules a JSON list.

**Sharing.** The folder's `SOUL.md` (at least 20 characters), `config.yaml`,
`skills/` and `cron/` are packed from an allowlist, every file is scanned for
what looks like a key, and one hit aborts the share before anything is sent.
The listing is uploaded with your handle; sharing the same name again sends
the manage token kept here, so it updates that listing instead of duplicating
it. The record of what this machine shared is `hermesmade/shared.txt` under the
Nodecode home, mode 600.

The only host this cell talks to is the shelf (`site`): no telemetry, no
background traffic.
