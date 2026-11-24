# nodecode-dreaming

Dreaming: nightly memory consolidation over what the organism keeps. A Nodecode rewrite of [itpartypattaya/hermes-dreaming](https://github.com/itpartypattaya/hermes-dreaming) by [itpartypattaya](https://github.com/itpartypattaya).

The original is a skill and a set of scripts a scheduler runs at night: a
deterministic pass reads facts and the conversations they came from, counts
how many **separate days** a fact came back on, scores what it finds, and
hands the model a small dossier - the few facts worth keeping, the memories
nobody has said in a while, the near-duplicates, and how full memory is. The
model decides and writes; the pass never rewrites memory, and a fact said on
one day only waits.

This cell is that pass, native and awake. What it corroborates are its own
sightings, one line per day a fact was said; what it reads is the organism's
own knowledge cell, the `(help 'name)` entries the index already lists. It
adds no store and asks for no key.

## Get it

Tell the agent "add the dreaming cell", or run `nodecode add nc://dreaming`.

## Set it up

Nothing is required: the cell runs from the first boot, with its ledger and
its diary in `dreaming/` under the Nodecode home. The section says how it
behaves:

```jsonc
{
  "dreaming": {
    "state_dir": "",          // empty: <home>/dreaming/
    "min_days": 2,            // separate days before a fact is proposed
    "forget_days": 60,        // days without a word, then "still relevant?"
    "budget_chars": 12000,    // characters of memory at which it says memory is filling
    "hour": 3                 // when the worker runs the pass
  }
}
```

## Use it

When the person states something worth keeping - a preference, who they are,
a standing rule, a fact about a project - the model records it once:

```
dream_sight {"text": "the operator runs on Thursdays", "type": "user"}
```

Said again on another day, it is a candidate. `dream_report` runs the pass
and answers in words, at most a page:

```
dreaming: 1 ready, 2 waiting, 0 already kept, 14 in memory, 31% full
ready -- said on separate days: keep with (define-memory name "one sentence" :type :user)
  the operator runs on Thursdays
    score 0.66: 2 separate days, 3 mentions -- last said 1 day ago
still relevant? -- nothing said or written in 60 days
  old-holiday-plan (2026-05-02)
nothing here was written by the pass: the model keeps, patches or forgets.
```

- **ready** - said on separate days and past the score gate. The model keeps
  it with the `define-memory` eval it already has, in the person's own words.
- **waiting** - one day is not a fact yet. Saying it again on another day is
  the whole point; nothing is lost meanwhile.
- **already in memory** - a memory already says it: the proposal names that
  memory instead.
- **near-duplicates** - two memories that say nearly the same thing. The pass
  merges nothing; the model decides, with the person.
- **still relevant?** - a memory nothing has said or written in `forget_days`.
  A question, never a deletion.
- **memory is filling** - the share of `budget_chars` the memory texts take.

`dream_reject {"text": "..."}` closes a proposal the person did not confirm,
and the pass stops showing it.

`/dream` in a shell is the same pass for a person: `/dream` runs it,
`/dream diary` reads the night's log, `/dream sight <fact>` records one,
`/dream reject <fact>` closes one, `/dream forget` lists what is stale.

At `hour` the worker runs the pass on its own, writes one section into the
diary (`DREAMS.md`, beside the ledger) and, when something is worth reading,
says so in one notice. `(help :dreaming)` carries the same rules to the
model.

## What it does not do

The original also reads a separate fact store and the raw conversation
transcripts to count mentions by itself, screens messages for secrets and
injected instructions, offers an optional cloud judge, and uses three
scheduled jobs to feed and sweep its store. Here a sighting is the
corroboration (the model records what the person said), the screen is the
organism's own, and the judge is off. See NOTICE.
