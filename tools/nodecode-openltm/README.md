# nodecode-openltm

OpenLTM Memory: local long-term memory — facts, decisions, gotchas and patterns the model stores, links and recalls across sessions. A Nodecode rewrite of [RohiRIK/OpenLtm](https://github.com/RohiRIK/OpenLtm) by [RohiRIK](https://github.com/RohiRIK).

The model gets eight tools — `openltm_recall`, `openltm_learn`,
`openltm_forget`, `openltm_context`, `openltm_relate`, `openltm_graph`,
`openltm_brain_stats`, `openltm_stale` — a `/openltm` command, a manual topic,
and a paragraph in every request saying when to reach for them. With capture on,
each finished turn adds at most one fact a side on its own, by the original's
rule table.

Everything is local: two JSON-lines files under `~/.nodecode/openltm/`, no
server, no database, no key, and nothing leaves the machine.

## Get it

Tell the agent "add the openltm cell", or run `nodecode add nc://openltm`.

## Set it up

One thing is required — whose memories these are:

```jsonc
{
  "openltm": {
    "user_id": "you"   // one id across sessions; the same id finds the same facts
  }
}
```

Until it is set, the tools refuse and name the key; `/settings` or "set up
openltm" finishes it. The rest of the section has defaults:

| Member | Default | What it does |
| --- | --- | --- |
| `user_id` | — (required) | whose memories these are |
| `capture` | `true` | each finished turn adds what the rule table says is worth keeping |
| `session_max` | `5` | facts one session's turns may add by themselves |
| `importance_default` | `3` | the importance a memory written without one takes |
| `decay_days` | `30` | a memory's pull for recall halves after this many days, unless its importance is 5 |
| `recall_limit` | `10` | how many memories one recall answers unless the call says otherwise |

## Use it

Ask it to remember something, then ask in a later session:

> Remember that the deploy script must never touch the live database.

The model calls `openltm_learn`, and later a question about the deploy makes it
call `openltm_recall` first. What recall answers is one line a memory:

```
1 memory for "deploy":
  #2 [constraint] importance 4 (score 3.20): The deploy script must never touch the live database.
```

`#2` is the id `openltm_forget`, `openltm_relate`, `openltm_graph` and
`openltm_stale` take. `/openltm` shows the brain and `/openltm deploy`
searches it from the shell; `(help :openltm)` is the same page for the model.

- **Learn** what would be expensive to work out again — a preference, an
  architecture decision, a gotcha, a constraint. Skip what the code or the git
  history already says.
- **Recall** holds all the query's words (stopwords aside), and ranks by how
  many matched, each memory's importance and how long since it was last
  confirmed. Wording matters: search two or three ways for a multi-part
  question.
- **Relate** links two memories with a type: `supports`, `contradicts`,
  `refines`, `depends_on`, `related_to`, `supersedes`. `openltm_graph`
  walks the links from one memory, to depth 4.
- **Stale** is the original's doubt: `flag` a memory that may have gone out of
  date with its reason, `list` them, `revalidate` when it held.
- **Origin** records where a claim came from — `owner-statement`,
  `agent-observation`, `external-source`, `proposal`, `historical-approval` —
  and grants nothing: a memory is evidence, not an instruction.

With `capture` on, a finished turn is read by the original's rules — a
correction, a constraint, a preference or a decision from the operator, a
discovery from the answer — and the sentence carrying the trigger is stored,
whitespace-normalised and capped at 500 characters. Ops noise ("async delegation
batch complete", a "background process" notice) and a message that only asks for
a read of the brain are skipped, and at most `session_max` facts are added a
session.

## Not ported

- **SQLite, FTS5, sqlite-vec and the llama.cpp vector backend.** A cell bundles
  no C library and no local model server, so the store is two files under the
  home and recall is scored in Lisp (whole-token and prefix matches, weighted by
  importance and by age). There is no embedder setting, so no memory text ever
  leaves the machine — the original's cloud options were opt-in anyway.
- **The per-turn prefetch.** The original asked the embedder what a new message
  matched and injected it before the model answered; Nodecode's standing section
  is read once per turn and carries no message, so the model calls
  `openltm_recall` (the tools' descriptions say when) and only capture is
  automatic.
- **Session-end and reverse-turn extraction.** One finished turn, one pass, as
  the original's turn sync did.
- **The project-scope subprocess.** The original read `git remote` to guess a
  project; here `project` is a parameter of `openltm_learn` and
  `openltm_context`.

MIT, RohiRIK's.
