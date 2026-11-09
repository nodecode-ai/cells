# nodecode-pushover

Pushover: a push notification on your phone when the agent needs you. A Nodecode rewrite of [lightx/pushover-hermes-plugin](https://github.com/lightx/pushover-hermes-plugin) by [lightx](https://github.com/lightx).

The model gets one tool, and a person gets the same verb from eval:

| What | Does |
| --- | --- |
| `pushover_notify` | A push to your phone or desktop: a message, a headline, a priority, a device, a sound, one link. |
| `/pushover` | What a push would use right now, or one test push to prove the keys work. |
| a hook on every live frame | With `notify_on` set, a push when a turn ends, when a turn fails, and when a question is waiting for an answer. |

Ask for "a push when the deploy is done" and the model calls `pushover_notify`; a long turn that finishes while you are away tells you so.

## Get it

Tell the agent "add the pushover cell", or run `nodecode add nc://pushover`.

## Set it up

Pushover needs an account (a one-time purchase per platform, then a monthly allowance of messages) and two keys:

1. **An application token.** At [pushover.net/apps](https://pushover.net/apps) → **Create New Application** → copy its API Token.
2. **Your user key.** On the [pushover.net](https://pushover.net) front page, after you log in.

Both are named, never written into the config: export them in the shell that starts Nodecode, and the section names the variables.

```sh
export PUSHOVER_APP_TOKEN=...   # the application's API token
export PUSHOVER_USER_KEY=...    # your user key
```

Then, in `/settings` (or in the `pushover` section of `config.jsonc`):

```jsonc
"pushover": {
  "app_token_env": "PUSHOVER_APP_TOKEN",
  "user_key_env": "PUSHOVER_USER_KEY",
  "notify_on": ["turn-end", "questions"],
  "title": "Nodecode",
  "device": "",
  "priority": 0,
  "timeout": 8
}
```

Optional, and off by default:

| Setting | What it does |
| --- | --- |
| `notify_on` | The events that push by themselves: `turn-end`, `turn-failed`, `questions`. Empty, nothing pushes on its own — only the tool and `/pushover test` send. |
| `device` | One of the account's devices by name; empty reaches every device. |
| `title` | The headline an automatic push carries. |
| `priority` | The priority of an automatic push, `-2` to `2`. `2` repeats until acknowledged. |
| `timeout` | How long one Pushover request may take, in seconds. |

## Use it

`/pushover` says what a push would use; `/pushover test` sends one, so you know the keys work before you rely on them. The model calls the tool when something is worth an interruption, and the manual it reads is `(help :pushover)`.

From eval, in the `pushover` package:

```lisp
(pushover:notify "the build is green" :priority 0)
```

`notify` is thin sugar over the tool's own body: it decodes the same contract and answers the same JSON. `(pushover:status)` is what the command answers.

Priorities are Pushover's: `-2` no banner and no sound, `-1` quiet, `0` normal, `1` high, `2` emergency — Pushover repeats an emergency push every `retry` seconds (60 by default) until it is acknowledged, and gives up after `expire` seconds (3600 by default). The cell fills both in when the priority is `2` and you did not.

### What it does not do

The original plugin is also a chat surface: a Hermes gateway platform that delivers the agent's replies to Pushover. Pushover has no inbound side to talk back through, so here it is one-way — the tool, the command and the lifecycle pushes. Desktop pop-ups are Nodecode's own (`notify` section of the config), so the cell does not repeat them.
