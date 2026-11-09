# nodecode-checkin

Idle check-ins: after your chat has been quiet for a while, one prompt goes into the session you
bound and its answer reaches you wherever that chat goes — with the wait doubling each time you
leave one unanswered, and nothing firing outside your waking hours. A Nodecode rewrite of
[capthvnsen/alans-way-agents](https://github.com/capthvnsen/alans-way-agents) by
[capthvnsen](https://github.com/capthvnsen).

## Get it

Tell the agent "add the checkin cell", or run `nodecode add nc://checkin`.

## Set it up

1. Open the chat you want to be checked in on — Telegram, Discord, a browser tab, the TUI — and
   send `/checkin on`. That binds the session you typed it in; `/checkin on SESSION` names one
   instead.
2. Tell it your offset: `/checkin utc -300` is New York in winter, `+60` Berlin in winter, `0`
   UTC. The rule keeps your local hours from it.
3. `/checkin status` says the wait, the hours, the offset and when the next one is due.

The same fields live in `/settings` under `checkin` (`session`, `base_minutes`, `active_start`,
`active_end`, `utc_offset_minutes`, `paused`, `morning_brief`, `voice`, `tick_seconds`); the
command writes them there, so a restart keeps them. A secret is not among them: the cell holds
none.

## Use it

| Command | What it does |
| --- | --- |
| `/checkin` or `/checkin status` | the rule in one line: the wait, the hours, the offset, the pause, the next check-in |
| `/checkin on [SESSION]` | check in on this chat (or that session) |
| `/checkin off` | stop checking in on anything |
| `/checkin now` | one check-in now, whatever the clock says |
| `/checkin less` / `normal` / `more` | a check-in after 4 / 2 / 1 hours of quiet |
| `/checkin pause` / `resume` | stop and start again, keeping the rule |
| `/checkin hours 9-23` | your waking hours, in your own offset |
| `/checkin utc -300` | your offset from UTC, east positive |
| `/checkin brief on` / `off` | one short brief a day, just after your hours open |
| `/checkin voice texting` / `default` | how your chats read |

The model has one tool, `checkin`. It reads the rule with `action: status`, and it can only make
check-ins **quieter**: `level less`, a higher `base_minutes`, `paused: on`, an earlier
`active_end` or later `active_start`, `morning_brief: off`, or a new `utc_offset_minutes`. A
change that would reach further is refused with `ask_user_to_send`, and the model tells you the
exact command — only you check in more often, resume, widen the hours or turn the brief on.

What a check-in is made of is the original plugin's: one of three moves — do one safe, reversible
thing and say what it did, offer one specific thing it could do, or ask about a decision you own —
built on one open thread from its memory of your goals, one to three short sentences, and exactly
`[SILENT]` when nothing is worth your attention.

At each turn's first round, the cell also hands the model the texting-voice note (plain words,
no preambles, headlines rather than logs, at most three sentences for a casual reply). Set
`voice` to `default` to leave your replies in the model's own voice.

It keeps one file, `checkins/state.sexp` under your Nodecode home, holding timestamps and a
count: the last exchange, the last check-in, how many went unanswered, the last brief. It holds
no conversation, and it makes no network call of its own — a check-in is a prompt in a session
of yours, and its answer is a normal turn.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, the prompts as data, and the clock arithmetic |
| `state.lisp` | the one state file, and the section's values as the cell runs on them |
| `cell.lisp` | the rule, the `checkin` tool, `/checkin`, the two hooks, `(help :checkin)` |
| `test/` | the lifecycle, the tool, the command and the rule, over a stubbed ingress and a fixed clock |

## What it does not do

The original plugin's workspace skills (browser errands, a companion desktop app over SSH), its
cross-bot digest directory and `cross_bot_ask` tool, and its send-time polish of a leading
"Certainly!" are not carried over; a check-in fires for the one session you bind, not for several
bots. See `NOTICE`.

## License

MIT, the original's: see `LICENSE` and `NOTICE`.
