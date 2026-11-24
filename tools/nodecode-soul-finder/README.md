# nodecode-soul-finder

Soul Finder: drafts and refines the SOUL.md personality file a channel reads before every message, through a short guided interview. A Nodecode rewrite of [RegeneratusLabs/Soul-Finder](https://github.com/RegeneratusLabs/Soul-Finder) by [RegeneratusLabs](https://github.com/RegeneratusLabs).

## Get it

Tell the agent "add the soul-finder cell", or run `nodecode add nc://soul-finder`.

## Set it up

Nothing is required. `~/.nodecode/config.jsonc` gains a `soul-finder` section you can leave empty:

- `soul_file` — the SOUL.md to work on; empty is the home's `SOUL.md`, the file Nodecode's channels read.
- `min_lines` (8) and `min_chars` (200) — what counts as a real SOUL.md rather than an empty seed. A hand-written one is left alone.
- `persona_lines` (60) — past this many non-blank lines the status call says persona, not voice.
- `nudge` (on) — offer a first-run SOUL.md once, when the home has none.

## Use it

- **No SOUL.md yet?** The cell says so once, at start: `/soul` drafts one. It never asks again unless you say yes to nothing and leave the file absent — the offer is marked as made.
- **A personality that reads like a style guide?** `/soul` audits the file you have: what is voice, what is project context that belongs elsewhere, what to cut.
- `/soul new`, `/soul refine` and `/soul status` pick the flow by name; bare `/soul` picks it from the file itself.
- The model reads the workflow with `soul_finder_guide`, checks where the file stands with `soul_finder_status`, and closes the flow with `soul_finder_complete` only after you approved the draft it showed you.

It writes nothing but two marker files of its own under `~/.nodecode/soul-finder/`; the SOUL.md is written by the agent's own file tools, after your approval. No network, no keys.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The cell: the `/soul` command, the three tools, the once-only offer. |
| `skill/SKILL.md` | The original's workflow skill, unchanged and read at run time by `soul_finder_guide`: audit, condense, inject personality, ship. |
| `test/` | The lifecycle and the new/refine decision, `/soul`, and the guide, offline. |

## License

MIT, Copyright (c) 2026 Josh: see `LICENSE` and `NOTICE`.
