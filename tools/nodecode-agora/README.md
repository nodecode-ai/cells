# nodecode-agora

Agora: a team that argues a motion in rounds, votes on it, and turns the adopted conclusion into tasks. A Nodecode rewrite of [yzy806806/agora](https://github.com/yzy806806/agora) by [yzy806806](https://github.com/yzy806806).

## Get it

Tell the agent "add the agora cell", or run `nodecode add nc://agora`.

## Set it up

Nothing is required and no key is needed: the cell writes one ledger file
under the home. Two settings are worth knowing:

```jsonc
{
  "agora": {
    "roles": ["architect", "developer", "reviewer"],  // who deliberates when a motion names nobody
    "max_speeches": 40,                                // the longest transcript one motion keeps
    "ledger": "agora/ledger.sexp"                       // where the file lives, under the home
  }
}
```

Leave `roles` empty and every motion must name its own participants, or the
call refuses and says so.

## Use it

A motion is a decision put to the team. The nine tools are the whole
protocol:

- `agora_raise_motion` — the question, the roles that deliberate it, how many
  rounds. It answers an id (`m-1`).
- `agora_speak` — one role's speech in the round it is in: `propose`,
  `support`, `oppose` or `question`.
- `agora_vote` — `for`, `against` or `abstain`, with a reason. A role's later
  vote replaces its earlier one, so the team may change its mind before the
  close.
- `agora_close_motion` — tallies: more for than against is adopted, anything
  else rejected, and the rationale is kept with it. A closed motion takes no
  more speech.
- `agora_get_motion`, `agora_list_motions` — the transcript and the board.
- `agora_create_task`, `agora_list_tasks`, `agora_close_task` — the work an
  adopted conclusion becomes. A task may cite a motion, and only a closed,
  adopted one: the task is then the decision executed.

`/agora` reads the board, and raises, shows, closes and tasks from the
command line. `(help :agora)` is the manual.

The same verbs are ordinary Lisp functions — `(agora:raise-motion "SQLite or
PostgreSQL?" :rounds 2)` — so a session can argue a motion from `eval` and a
layer can reword what it says.

### What it keeps

One file, rewritten whole after every verb, readable and editable:

```
~/.nodecode/agora/ledger.sexp
```

Every motion, every speech, every vote and every task is in it, so a restart
loses nothing and a person can read what the team decided and why.

### The team is the session

The original spawned a Hermes profile per role, each a real agent, and chaired
them from a heartbeat cron. Here the sessions do the talking: one session may
speak for each role in turn, several may take one role each, and a team of
sessions arguing one motion only needs each of them to call `agora_speak` with
the role it holds. The ledger is what they share, and it outlives all of them.
