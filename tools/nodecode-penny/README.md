# nodecode-penny

Penny: your core memory, persona and private notes, read and written by the model. A Nodecode rewrite of [HelperLabs/penny-hermes](https://github.com/HelperLabs/penny-hermes) by [HelperLabs](https://github.com/HelperLabs).

## Get it

Tell the agent "add the penny cell", or run `nodecode add nc://penny`.

## Set it up

The cell needs the access token Penny's sign-in hands out, and nothing else.
Either export it:

```sh
export PENNY_TOKEN=...
```

or set it in `/settings` (or the page's Settings tab) under `penny`: `token`
for the token itself, `token_env` for the name of the variable holding it,
or `token_file` for a file holding it — a Penny CLI's `auth.json` is read as
it is. A token pasted into the panel is kept in a 0600 file under
`~/.nodecode/secrets/`; the folder holds no secret.

Everything the model does goes to Penny's own endpoint,
`https://app.mypenny.ai/mcp`. `/penny` alone shows the endpoint, whether a
token is set, and the core memory Penny keeps; `(help :penny)` is the
model's guide to the tools and how to use them.

## Use it

Ask for what you want; the model picks the tool:

| Tool | Use |
| --- | --- |
| `penny_profile` | the core memory and assistant persona Penny keeps for you |
| `penny_search` | the private notes nearest a question — past decisions, work, context |
| `penny_remember` | save one durable fact or decision, with three questions it answers |

Or from the keyboard: `/penny how do we deploy` runs one search.

Recall is the model's call, not a prefetch: it reads the profile before
answering from your preferences or history, and searches when the answer
depends on earlier work. Saved notes are yours in Penny — the same notes any
other Penny client sees.

## What it does not do

The original also injected memory into every turn's prompt automatically and
ran its own OAuth login. Here the model asks for memory when it needs it, and
the token comes from the section. Penny's scheduled work, spaces sharing and
self-update were outside the plugin too.

## Credit

MIT, Copyright (c) 2026 Helper Labs. See [NOTICE](NOTICE) for what was
rewritten from the plugin and what was left out; [LICENSE](LICENSE) is the
original's, unchanged.
