# nodecode-inbox

One read-only view of everything in flight: what needs attention, what is running, answers not yet delivered, what is scheduled and the sessions seen most recently. A Nodecode rewrite of [NousResearch/hermes-labs-plugins](https://github.com/NousResearch/hermes-labs-plugins) by [NousResearch](https://github.com/NousResearch).

## Get it

Tell the agent "add the inbox cell", or run `nodecode add nc://inbox`.

## Set it up

Nothing is required. The `inbox` section has one member, `limit`: how many rows each section prints (20 by default).

```jsonc
"inbox": { "limit": 20 }
```

## Use it

Ask the agent what is going on, what is pending, or what ran while you were away -- it calls the `inbox` tool -- or type `/inbox` yourself. The command takes a section name: `/inbox attention`, `/inbox progress`, `/inbox finished`, `/inbox scheduled`, `/inbox sessions`; `/inbox` alone is all of it.

```
  ◆ Needs attention (2)
    ⚠ notice nodecode-cron [error]  cron: job nightly-backup failed
    ⚠ session [cli]  Plan the offsite  waiting on you: which venue?  (5m ago)

  ◆ In progress (1)
    ◐ eval 7  [running]  (sh "make test")

  ◆ Scheduled (1 active job(s))
    ⏱ cron 'morning-digest'  next in 3h12m

  ◆ Open surfaces (2)
    ◎ session [cli]  Plan the offsite  (5m ago)
    ◎ session [tui]  Refactor billing  (1m ago)
```

| Section | What appears |
|---|---|
| Needs attention | What a cell last said under a standing-notice key, a session whose answer waits on the person, and a cron job whose last run failed or that was paused. |
| In progress | The evaluations still running, each with the id `(eval-status N)` takes. |
| Finished, result not yet delivered | A cron answer the session that scheduled it has not been sent yet. |
| Scheduled | The active cron jobs and how far away each next fire is. |
| Open surfaces | The sessions seen most recently, and how long ago. |

The first two sections always print, so an empty home reads as all clear. A section with nothing in it is otherwise left out, and every source that could not be read -- no store, no cron jobs yet, an older image -- is named in the `Not read` footer instead of failing the read.

## What it reads

Four sources, all read-only and all through their public readers; nothing is changed and nothing is written.

| Source | Store |
|---|---|
| Standing notices | The notice board the cells speak on (`nle:notice` with a key). |
| Sessions | The session directory in the store: titles, sources, when each was last touched, and which answers wait on the person. |
| Cron | The `cron` cell's registry row, when jobs exist: state, last outcome, next fire, an answer still owed. |
| Evaluations | This image's evaluation registry, the same one behind `(list-evals)`. |

A script can read the snapshot itself: `(nodecode-inbox:snapshot)` answers a plist (`:attention`, `:in-progress`, `:finished`, `:scheduled`, `:sessions`, `:sources`), and `(nodecode-inbox:inbox-text :only "attention")` the text.

## License

MIT, Nous Research's (see `LICENSE` and `NOTICE`).
