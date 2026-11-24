# nodecode-raise

Raise: a draft-only fundraising desk that ranks investors on cited evidence and gates and lints every outreach draft. A Nodecode rewrite of [GenesisEngine-Triolith/raise](https://github.com/GenesisEngine-Triolith/raise) by [GenesisEngine-Triolith](https://github.com/GenesisEngine-Triolith).

Raise is for the season a founder spends raising. It keeps one pipeline of
investors, the evidence each ranking stands on, and the log of who was written
to. The model proposes and writes; the code checks. A score counts only the
dimensions that cite evidence, a route moves warmest-first and waits after a
pending ask, and a draft is refused while a number in it comes from nowhere.
Nothing is ever sent: the cell stops at a draft the founder sends themselves.

## Get it

Tell the agent "add the raise cell", or run:

```sh
nodecode add nc://raise
```

## Set it up

No key, no account: the pipeline is a JSON file under the home
(`raise/pipeline.json`) and the desk works offline. The `raise` section of
`config.jsonc` is optional; every member has a default:

| Member | Default | What it does |
| --- | --- | --- |
| `focus_size` | 10 | how many investors `raise_focus` lists |
| `hold_workdays` | 3 | working days a pending ask holds every later route |
| `cap_per_channel` | 3 | touches on one channel before the route moves on |
| `cap_window_days` | 180 | the window those touches are counted in |
| `email_mode` | none | where a draft is meant to land; Raise only ever drafts |
| `draft_style` | empty | skills a draft must follow, by name |

## Use it

The model calls the `raise_*` tools; the operator types `/raise`:

- `/raise` - the pipeline in numbers, and a panel of what to do next.
- `/raise next` - at most five actions today, replies first.
- `/raise focus` - the ranked investors worth working now, best first.
- `/raise Ada Lind` (or `show`) - one investor: the score a dimension, the
  evidence cited, the log, the route and the gate.
- `/raise can Ada Lind` - whether a draft may go out now, and on which channel.
- `/raise export csv` - the pipeline as CSV, for the founder's own CRM.

The tools, in the order a founder meets them:

| Tool | What it does |
| --- | --- |
| `raise_profile`, `raise_claim` | the company, and the claims a draft may quantify (verified, unverified, projection) |
| `raise_investor`, `raise_import` | one investor, or a list from rows or CSV, deduped by person and firm |
| `raise_evidence` | one cited fact about an investor: a deal, a thesis page, a post. The source url is required |
| `raise_rank` | 0-10 a dimension with cited evidence ids; the code checks the citations, computes reachability from the route and the total |
| `raise_focus`, `raise_show` | who to work now, and one investor in full |
| `raise_can_draft` | the gate: mode (cold, follow-up, reply), channel, reasons, the route and its caps |
| `raise_lint` | the code checks on a draft: numbers, personalisation, and the style tells |
| `raise_log` | a touch, a reply, a pass, an in-talks, a committed cheque, a note |
| `raise_brief`, `raise_export` | today's actions, and the pipeline as CSV or rows |

Calls are also Lisp verbs in the `raise` package, through eval:

```lisp
(raise:focus 5)                       ; the five worth working now
(raise:can-draft "Ada Lind")          ; may a draft go out, and where
(raise:lint-draft TEXT :investor "Ada Lind" :channel "email")
(raise:brief) (raise:status-text)
```

Each answers a string; a refusal is `ERROR: RAISE-ERROR` with the reason.
`(help :raise)` carries the same on one page.

## Tests

```sh
.github/test-cell.sh tools/nodecode-raise
```

Ten tests over a scratch home: the lifecycle (start, stop, a second start, a
vetoed section, a refused one), the pipeline file, a ranking that zeroes an
uncited dimension, the gate holding a pending touch and answering a reply, the
lint blocking a number nobody can cite, and the command and the brief. The desk
has no network; nothing is stubbed but the home.
