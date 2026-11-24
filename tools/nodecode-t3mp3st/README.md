# nodecode-t3mp3st

A Nodecode rewrite of the War Room client of [elder-plinius/T3MP3ST](https://github.com/elder-plinius/T3MP3ST) by [Pliny the Elder](https://github.com/elder-plinius): start a mission at named targets, watch its phase and progress, read what the run found, and clear the approval gates it stops at.

T3MP3ST is an autonomous red-teaming platform by [Pliny the Elder](https://github.com/elder-plinius) - a team of operator agents that runs a mission against a target. It is **a separate program under AGPL-3.0**. This cell ships none of its code: it speaks to the War Room T3MP3ST serves, over the same HTTP API its own UI reads, so the licence stays where it belongs and this cell stays MIT. See NOTICE.

## Get it

Tell the agent "add the t3mp3st cell", or run `nodecode add nc://t3mp3st`.

## Run T3MP3ST first

The cell drives a War Room; it does not install one. In a T3MP3ST checkout:

```bash
npm install
npm run server          # → http://127.0.0.1:3333/ui/
```

Then connect a brain in its **Settings** (a local agent such as Claude Code or Codex, or a provider key). The cell works the moment that server answers; nothing else is needed.

## Set it up

One setting, and only if your War Room is somewhere else: `url`, in the `t3mp3st` section of `~/.nodecode/config.jsonc`. `timeout_seconds` bounds one call.

```jsonc
{ "t3mp3st": { "enabled": true, "url": "http://127.0.0.1:3333", "timeout_seconds": 20 } }
```

## Use it

Ask for a run at targets you are authorized to test:

> Start a T3MP3ST mission on `10.20.0.5` and `app.example`, named "nightfall". Written authorization from the owner, weekdays only.

The model has five tools:

| Tool | What it does |
|---|---|
| `t3mp3st_status` | the mission's name, state, phase, progress, ticks, targets and operators - or that the server is up with none |
| `t3mp3st_launch` | start a mission at one or more targets, optionally naming a provider and model, or a local repo to read white-box |
| `t3mp3st_findings` | what the mission's vault holds, plus the credential records (metadata only - the War Room redacts the secrets) |
| `t3mp3st_approvals` | list the gates, or clear one: `action=approve|reject` with its id |
| `t3mp3st_control` | `stop`, `pause` or `resume` the mission |

And `/t3mp3st` for the operator: `/t3mp3st` (status), `/t3mp3st launch <target>`, `/t3mp3st findings`, `/t3mp3st approvals`, `/t3mp3st approve <id>`, `/t3mp3st stop`.

## What it is not

It is not a copy of T3MP3ST, and it does not replace its War Room UI. Deployment plans, the arsenal catalogue and the operator runbooks are still the UI's. This is the seam an agent needs: launch, watch, read, clear a gate - in the conversation, and in a form a model can act on.

## Credits

T3MP3ST by Pliny the Elder - https://github.com/elder-plinius/T3MP3ST (AGPL-3.0). This cell is an independent client and is not endorsed by that project. MIT, see LICENSE.
