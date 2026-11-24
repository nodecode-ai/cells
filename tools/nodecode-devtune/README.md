# nodecode-devtune

DevTune: what AI answers say about a product, and which tools coding agents pick. A Nodecode rewrite of [devtunehq/skills](https://github.com/devtunehq/skills) by [devtunehq](https://github.com/devtunehq).

## Get it

Tell the agent "add the devtune cell", or run `nodecode add nc://devtune`.

## Set it up

Make a project API key under **API Keys** in the DevTune account sidebar, then paste it into `api_key`, or leave that empty and let the cell read the variable `api_key_env` names (`DEVTUNE_API_KEY` by default).

```jsonc
{ "devtune": { "api_key": "your-key", "window_days": 30 } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `api_key` | — | the project API key; one of `api_key` or a set `api_key_env` is required |
| `api_key_env` | `DEVTUNE_API_KEY` | the variable read when `api_key` is empty |
| `project_id` | empty | the project to read; empty, the key's own connection answers it |
| `base_url` | `https://devtune.ai/api/v2` | the REST root; only a proxy or a mirror needs another |
| `window_days` | `30` | the measurement window a read that names none asks for: 30 or 90 |
| `timeout_seconds` | `30` | seconds one read may take |

A section that names no key refuses the start and says so, and the tool is not
registered until one is set. Then `(restart-cells "nodecode-devtune")`, or set it in
`/settings`. Say `/devtune` to see which project the key reaches and what it can read.

## Use it

Ask what AI answer engines say about your product, who they cite instead, what AI
crawls and refers to your site, what to fix next, or whether a fix that shipped moved
the numbers. Say "which tool would Claude Code pick to add auth?" and the model reads
the Agent Picks reports instead.

The model calls `devtune_read` with one report name; the answer is DevTune's own JSON
under one line naming what was read. From eval, the same call:

```lisp
(devtune:reports)                                  ; every report this cell reads
(devtune:report :connection)                       ; the project the key reaches
(devtune:report :visibility-summary)               ; the section's window
(devtune:report :visibility-diff :window-days 90)  ; and how it moved
(devtune:report :actions)                          ; what to fix next, ranked
(devtune:report :action-brief :id "act_123")       ; one action's brief
(devtune:report :agent-picks-summary :setup "auth")
(devtune:report :agent-picks-runs :cursor "c_2")   ; the page after the last one
```

| The question | The report |
| --- | --- |
| How visible the product is in AI answers | `visibility-summary`, `visibility-diff` |
| Who gets cited instead | `competitive-position`, `citation-domains`, `citation-pages` |
| What an AI platform actually said | `visibility-responses`, then `visibility-response` |
| Topics a competitor is cited for and you are not | `content-gaps`, then `content-gap-evidence` |
| AI crawlers, answer fetches, AI referrals | `traffic-summary`, `traffic-platforms`, `ai-referrals` |
| Pages AI keeps asking for that do not exist | `404-demand` |
| What to fix next | `actions`, then `action-brief` |
| Site readiness for AI | `audit-summary`, `audit-findings` |
| Whether shipped fixes worked | `outcomes-ledger` |
| Which tool coding agents pick | `agent-picks-summary`, `agent-picks-cells`, `agent-picks-history` |
| Individual agent runs, and what to fix | `agent-picks-runs`, `agent-picks-run-trace`, `agent-picks-actions` |
| A test's status, its schedule, what to set up | `agent-picks-test`, `agent-picks-schedule`, `agent-picks-catalogue` |

`(help :devtune)` answers the guidance the two skills carried, and every request
carries one line of it: brand awareness, citation presence and share of voice are
percentages from 0 to 100 while Agent Picks rates are fractions from 0 to 1, a
citation is not a mention, a `null` was not measured rather than zero, and an outcome
shows what moved after a fix, not that the fix caused it. `*primer*` is the text, so a
layer can reword it.

This cell reads. It creates, updates, archives and deletes nothing, starts no test and
spends no DevTune credit, so no approval is needed before a read and a model cannot
change a project through it. The write and credit-spending calls of the original's
hosted MCP server are not here.

## License

MIT. The original is Copyright (c) 2026 DevTune: see `LICENSE` and `NOTICE`.
