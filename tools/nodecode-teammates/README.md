# nodecode-teammates

Teammates: a named roster of subagents the agent hands work to, then checks, waits on, steers and stops. A Nodecode rewrite of [100yenadmin/hermes-teammates](https://github.com/100yenadmin/hermes-teammates) by [100yenadmin](https://github.com/100yenadmin).

## Get it

Tell the agent "add the teammates cell", or run `nodecode add nc://teammates`.

## Set it up

No key of its own. Write the roster in the `teammates` section of
`~/.nodecode/config.jsonc` (`/settings`, or the file; `config.example.jsonc`
has the shape):

```jsonc
"teammates": {
  "teammates": {
    "reviewer": {
      "description": "Slow and careful: reads a diff before anyone merges it.",
      "instructions": "You review changes. For each problem give the file and the line.",
      "provider": "anthropic", "model": "claude-opus-5", "reasoning_effort": "high"
    },
    "scout": {
      "description": "Cheap and quick: lookups, summaries, finding a caller.",
      "instructions": "You answer one lookup at a time and name where the answer came from."
    }
  },
  "seconds": 900, "wait_seconds": 60, "max_live_runs": 4
}
```

| Key | Meaning |
| --- | --- |
| `teammates.<name>.description` | The one line the agent reads to choose a teammate. Required. |
| `teammates.<name>.instructions` | The paragraph a run of it works under. Optional. |
| `teammates.<name>.provider`, `.model` | What a run of it runs on. Optional: give both or neither, and without them it follows the assigning session's model. |
| `teammates.<name>.reasoning_effort` | Optional, with `provider` and `model`. |
| `seconds` | How long one teammate turn may run before it is stopped (default 900). |
| `wait_seconds` | How long `teammate_wait` blocks by default (default 60). |
| `max_live_runs` | How many teammate runs one session may have in flight (default 4). |

A name is lowercase letters, digits, `-` or `_`, up to 32, starting with a
letter. An entry with no description, or one that names a provider without a
model, is skipped rather than guessed at, and the cell says which and why in a
standing notice; it is read when the cell starts, so
`(restart-cells "nodecode-teammates")` is what a rewritten roster needs.

## Use it

Ask for work to go to a teammate by name:

```
teammate_assign(teammate: "reviewer",
                goal: "Review the diff on the working tree and say whether it merges.",
                context: "The patch touches src/http.lisp; the tests pass.")
```

- A run is a **session of its own** under the session that assigned it: its
  own transcript, the same folder, listed on `/sessions` under the asker. It
  never sees the assigning conversation, so the goal and the context you pass
  are the whole of what it knows beyond its instructions.
- `teammates_roster` lists the roster and this conversation's runs with their
  state; `teammate_check(run)` reads one, with its answer once it is done;
  `teammate_wait(runs, mode, seconds)` blocks until they are at rest.
- `teammate_message(run, text)` steers the turn a run is on, or gives it its
  next turn when it is at rest. `teammate_stop(run)` cancels the turn; the
  session and what it wrote stay.
- Assigning again with `continue` set to a run id puts the new goal to that
  same session, so its earlier work is there.
- Only names and descriptions are in the tool's schema: the provider and the
  model are never tool parameters, so the agent cannot invent a teammate.

`/teammates` prints the roster and this conversation's runs; `/teammates NAME`
prints one teammate, its instructions and its runs.

## What is not here

- The Kanban lane and its review handoff: a run is a session, not a card.
- Per-teammate toolset narrowing: a run has the tools of a session.

## License

MIT. The original plugin is Copyright (c) 2026 Electric Sheep: see `LICENSE` and `NOTICE`.
