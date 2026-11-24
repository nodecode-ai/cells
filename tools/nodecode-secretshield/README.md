# nodecode-secretshield

SecretShield: stops credentials from passing through tool calls, and keeps a local incident ledger. A Nodecode rewrite of [Whydiriki700/secretshield](https://github.com/Whydiriki700/secretshield) by [Whydiriki700](https://github.com/Whydiriki700).

A model that has a key in hand tends to put it where the call goes: a `curl`
with `Authorization: Bearer ...`, a `password = ...` in a file it writes, a
private key pasted into a command. SecretShield reads every tool call's
arguments before the tool runs and stops the call while the credential is still
in it. It is a table of patterns, compiled locally: no service, no model, no
network, no key.

## Get it

Tell the agent "add the secretshield cell", or run `nodecode add nc://secretshield`.

## Set it up

Nothing has to be set: with an empty section the shield runs in `enforce` mode
and reads every call's arguments for the credential shapes it knows. The
section changes what it does:

- `secretshield.mode` — `enforce` (the default; the call does not run),
  `redact` (the call runs with `[REDACTED by SecretShield]` where the
  credential was), `warn` (the call runs untouched and the attempt is
  recorded), `off` (the cell observes nothing).
- `secretshield.disabled_kinds` — detector kinds to skip, as the table
  spells them (`openai_key`, `jwt`, `assigned_secret`, ...). A kind the
  table does not carry is refused at start, so a typo shows up as the
  folder's `REFUSED` line instead of a shield that skips nothing.

```jsonc
"secretshield": { "mode": "redact", "disabled_kinds": ["assigned_secret"] }
```

## Use it

Nothing to type. A call that carries a credential is answered in its place:

```
ERROR: blocked by SecretShield: the arguments of this terminal call carry
credential material (openai_key (sk-a****, 43 chars) in command). Credentials
must not be passed through tool calls: remove it, use the tool's own
authenticated configuration, and retry.
```

The credential itself never lands anywhere: an incident records the kind, its
length and its first four characters, one JSON line apiece in
`~/.nodecode/secretshield/incidents.jsonl`. A credential a tool *printed* is an
incident too (recorded, not blocked), and in each session's first round the
model is told once what the shield does, so a refusal reads as a rule and not
as a mystery.

Three ways to ask for it directly:

- `shield_status` — the tool: the ledger as JSON, for the model.
- `/secretshield` — the mode, how many calls were stopped or rewritten, and
  the newest incidents; `/secretshield <text>` previews any text.
- `(secretshield:scan-text "…")` and `(secretshield:redact-text "…")` from
  eval, and `(help :secretshield)` for the manual.

### What it reads

Private keys in PEM; Anthropic, OpenAI, GitHub, AWS, Google, Slack, Stripe,
SendGrid, npm and PyPI keys; Telegram and Discord bot tokens; JWTs; bearer
credentials; a password in a URL's userinfo; and one assignment context
(`api_key = ...`, `password: ...`, `client_secret = ...`). The table is the
original's, unchanged.

### What it does not do

It cannot rewrite a reply the model has already streamed: a credential quoted
in the reply itself is caught on its way back out as another call, or by
`/secretshield <text>`. And it is a speed bump, not a boundary: a format
nobody wrote down goes through, as it did in the original.

## Test it

```sh
.github/test-cell.sh tools/nodecode-secretshield
```

The tests run in a scratch image with no network. Every credential in them is
built at run time from a prefix and a deterministic fill, so the repository
holds no credential-shaped literal.
