# nodecode-muse

Muse: a companion that keeps your goals, your passing interests, a quiet Feed to read and one-line reminders. A Nodecode rewrite of [xiaohei-info/hermes-muse](https://github.com/xiaohei-info/hermes-muse) by [xiaohei-info](https://github.com/xiaohei-info).

## Get it

Tell the agent "add the muse cell", or run `nodecode add nc://muse`.

## Use it

Say what you are trying to finish. "I want to pick a bicycle by month-end" becomes a goal with what done means; "I keep reading about cargo bikes" becomes an interest that expires in two weeks unless you mention it again. A reminder you ask for is kept, and the next request after it comes due carries it as one line -- then it is marked said, and never said twice.

The model has one tool, `muse_manage`, with the original's action names and its one `data` object per call:

| Action | What it does |
| --- | --- |
| `context` | the store: `goals`, `topics`, `ideas`, `feed`, `reminders`, `preferences`, `status` |
| `status` | counts, what is due, where the store is |
| `goal_create`, `goal_update` | keep a goal with its completion criteria; close it with a note, or cancel it |
| `interest_record` | a passing topic, expiring after `topic_days` |
| `idea_add` | a suggestion, never work started |
| `feed_add`, `feed_list`, `feed_update`, `content_read` | the Feed: articles with sources and a reason they matter |
| `notification_add`, `notification_resolve` | reminders: kept, said once, then done, dismissed, stopped or snoozed |
| `preferences` | the Feed brief that steers what gets written |
| `forget` | remove one record |

From eval, the same store through verbs: `(muse:goals)`, `(muse:goal "pick a bicycle" :criteria "one is ordered" :deadline "2026-11-30")`, `(muse:goal-done "goal-1" "ordered and paid for")`, `(muse:feed-add "Cargo bikes, honestly" "..." :sources '("https://example.org/a") :why "you are choosing one")`, `(muse:remind "renew the passport" :due "2026-11-20")`, `(muse:context "feed")`. The model reaches the same store through `muse_manage`.

`/muse` opens the panel: what you are finishing, the newest articles, and the two lines worth typing (`done`, `read`). `/muse done goal-1`, `/muse read feed-2`, `/muse feed`, `/muse goal TITLE`, `/muse remind TEXT`, `/muse forget ID` run by hand.

The store is one JSON file, `muse/state.json` under the home, written whole and renamed in, so it survives restarts and any tool can read it. Nothing is sent anywhere: no server, no key, no telemetry.

Three settings, all optional, in the `muse` section: `topic_days` (14) how long an interest lasts, `feed_chars` (4000) how much of one article `content_read` answers, `remind_chars` (240) how much of a reminder the round carries.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the store, the actions, `muse_manage`, `/muse`, the round's reminder |
| `test/` | the slice: lifecycle, the tool over a real store, `/muse`, the reminder said once |

The Feed is written by whoever authors an article -- the model, on request, or a script through `(muse:feed-add ...)` -- and read by you; this cell keeps no schedule of its own, so nothing runs while a session idles.

## License

MIT. The original is Copyright (c) 2026 xiaohei-info: see `LICENSE` and `NOTICE`.
