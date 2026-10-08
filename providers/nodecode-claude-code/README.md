# nodecode-claude-code

A Nodecode add-on that runs Claude models through your own logged-in Claude
Code CLI. Pick `claude-code/<model>` in `/models`, and each model round goes
out with the login `claude` already has.

Nodecode keeps the agent. Its turn loop, tools, context engine, retries and
usage record work the same as on any other provider. The CLI only writes the
request:

1. Nodecode builds the round's Messages request, the same way it does for
   Anthropic.
2. The add-on starts `claude -p` in stream-json mode with its own tools off.
   It hands the CLI the round's system prompt, history and tools, and points
   `ANTHROPIC_BASE_URL` at a loopback relay only that run knows.
3. The CLI writes the request it would make, with its own headers. The relay
   keeps it and the CLI is stopped, so the CLI itself never sends anything
   to Anthropic.
4. Nodecode sends those exact bytes to `api.anthropic.com` and reads the
   stream with its own Anthropic parser. Tool calls come back under
   Nodecode's tool names and run in Nodecode, never in the CLI.

It follows NousResearch's
[hermes-plugin-claude-subscription-directsdk](https://github.com/NousResearch/hermes-plugin-claude-subscription-directsdk).

## Before you use it

- **Anthropic's terms.** The Claude Agent SDK documentation says third-party
  products may not offer claude.ai login or plan rate limits without
  Anthropic's approval. This add-on is for running your own CLI on your own
  machine. Read the current terms yourself before relying on it.
- **Billing.** Whether a round draws from your plan or from extra usage is
  decided by Anthropic's service. Turn off extra usage in your account if you
  don't want overage charges. Nodecode puts no price on claude-code rounds:
  none is billed per token, so the Dashboard shows no cost for them.
- **Models.** Models with windows over 200K run on their 1M route. On some
  plans a 1M route, and Fable, draw from usage credits.

## Install

Linux and macOS.

Install [Claude Code](https://claude.com/claude-code), run `claude` once and
`/login`. Then copy this folder into
`~/.nodecode/addons/nodecode-claude-code/` and restart Nodecode (or run
`(restart-addons)`). When no `claude` is on PATH, a notice says so.

## Configure

This is optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "claude-code": {
    "command": "claude",
    "models": ["claude-sonnet-5", "claude-haiku-4-5"],
    "timeout_seconds": 120
  }
}
```

See [`config.example.jsonc`](config.example.jsonc).

## What it costs in time

Every round starts a fresh `claude` process, about 0.4 s. The CLI then
replays the session's history, about 20 ms for each earlier user turn. The
CLI adds its own per-request context to the newest turn: the date, the
account's email, and a short environment note naming the session's folder.
The add-on moves the CLI's cache marker onto the part of the history the
next round sends again unchanged, so earlier rounds are still read from
cache.

## Test

From a Nodecode checkout's `src/`:

```sh
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(push #p"/path/to/addons/nodecode-claude-code/" asdf:*central-registry*)' \
  --eval '(asdf:test-system :nodecode-claude-code)'
```

The tests never start the real CLI and never reach Anthropic. A whole round
runs against `test/fake-claude.py`, a stand-in that speaks the CLI's
stream-json and needs `python3`.

MIT licensed.
