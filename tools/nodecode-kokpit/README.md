# nodecode-kokpit

Kokpit: your own Markdown vault — capture notes, tasks and journal lines, read your threads, get a brief, and a night pass. A Nodecode rewrite of [sahindemirbas/kokpit](https://github.com/sahindemirbas/kokpit) by [sahindemirbas](https://github.com/sahindemirbas).

One folder of plain Markdown, kept by you and read by the agent. Kokpit never
edits, moves or deletes a note: a capture is a new file, one line under `##
Open`, or one line at the end of the day's journal. Night proposals quote the
exact line they came from, so you can check them in the file.

The model gets five tools:

- `kokpit_brief` — the day as one short text: open tasks and what is overdue,
  the thread items marked on you, the last session's open points, and the
  night proposals.
- `kokpit_brain_search` — words looked for across the vault's Markdown, with
  the path, the line number and one snippet each.
- `kokpit_threads` — where you left off, with the owner of every open item,
  and the latest session card.
- `kokpit_capture` — a note (a new file in `inbox/`), a task (one line under
  `## Open` in `To-Do.md`), a journal line (in `daily/YYYY-MM-DD.md`), or a
  receipt that closes a result: one card in `Last-Session.md` and one short
  file in `receipts/` pointing at what the session wrote.
- `kokpit_night` — the night pass: proposals read off the vault, each quoting
  its source line; accept writes a To-Do line, dismiss puts it away.

And `/kokpit` for you: `brief`, `threads`, `search`, `note`, `todo`, `j`
(journal), `done` (a receipt), `night`, `night run`, `add <id>`, `skip <id>`.

Nothing leaves the machine: no network, no model call. The vault is the only
thing it writes to.

## Get it

Tell the agent "add the kokpit cell", or run `nodecode add nc://kokpit`.

## Set it up

The one thing it needs is the folder of Markdown notes, in the `kokpit`
section (the setup panel, `/settings`, or `config.jsonc`):

```jsonc
{
  "kokpit": {
    "vault": "~/Kokpit"
  }
}
```

Everything else has a default and can be moved: `threads_file`
(`Threads.md`), `todo_file` (`To-Do.md`), `last_session_file`
(`Last-Session.md`), `inbox_dir` (`inbox`), `receipts_dir` (`receipts`),
`journal_dir` (`daily`), `stale_days` (7, how long a thread may sit before
the night pass proposes a next step) and `brief_chars` (3500). A file name is
read inside the vault; when it is not at the root, the single match found
below the vault is used, and two matches are a refusal naming them.

A vault that is not a folder is refused at start, in words that name the
setting. Nothing is created for you: make the folder, or point `vault` at the
notes you already have.

## Use it

```
/kokpit note Ask about the hedge trimmer
/kokpit todo call the glazier due:2026-05-04
/kokpit j watered at six
/kokpit brief
/kokpit night run
/kokpit add 3f9a1c
/kokpit done Decided to water at 06:00, note saved
```

The vault formats are the original plugin's, unchanged, so a vault written by
Kokpit or by hand reads the same here:

```markdown
## Active Threads

### Kitchen shelf
2026-10-01
Where we left off: waiting on the boards.
Open: on you: pick the paint; Bende: order the brackets
Source: [[notes/kitchen]]
```

An `Open:` item may name its owner (`on you`, `on me`, `Sende`, `Bende`,
`Başkası`, or any name), and an item with no owner is on you when it says
approve, decide or waiting on you. `To-Do.md` counts only its `## Open`
section (or every section but the Done ones), reads `- [ ] text (due:
YYYY-MM-DD)`, and leaves ticked lines alone.

What the original plugin reads from the Kokpit application's database —
calendar, weather, events, job listings, AI token usage, job health, training
logs — is not here: this is the vault half its README says works on its own.
The optional host-model night pass is not here either; the rules alone answer
offline.

`help :kokpit` in a session answers the same, in short.
