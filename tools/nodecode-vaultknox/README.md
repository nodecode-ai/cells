# nodecode-vaultknox

VaultKnox: API keys, tokens and passwords redacted from chat in both directions. A Nodecode rewrite of [Ufonik88/Hermes-VaultKnox](https://github.com/Ufonik88/Hermes-VaultKnox) by [Ufonik88](https://github.com/Ufonik88).

A key a user pastes into a chat, a token a model echoes back, a password a tool
result printed: the plugin this rewrites read the text of a Hermes conversation
with 28 secret detectors and replaced every match before it reached the session
store or the user. This cell does the same to the text Nodecode sends: the
detectors run over every message of a request on its way to the provider, and
an assistant turn that asks you to paste a key is rewritten into safe guidance.

## Get it

Tell the agent "add the vaultknox cell", or run `nodecode add nc://vaultknox`.

## Set it up

Nothing is required: with an empty section every detector runs, and the cell is
on as soon as its folder is.

- `vaultknox.detectors` — the detectors to run, by key. Leave it out for all of
  them, or name a few: `["openai", "anthropic", "aws-key-id", "github-pat",
  "private-key", "jwt", "password"]`. A key the registry does not know is
  refused at start, with the registry's names in the refusal.
  `/vaultknox` prints the live list.

```jsonc
"vaultknox": { "detectors": ["openai", "anthropic", "github-pat", "private-key", "jwt"] }
```

The registry, in the order the plugin carried it: `openai`, `github-pat`,
`github-oauth`, `github-fine-grained`, `github-impersonation`, `github-refresh`,
`anthropic`, `aws-key-id`, `aws-secret`, `api-key`, `secret-key`,
`access-token`, `auth-token`, `password`, `bearer`, `slack`, `stripe-secret`,
`stripe-publishable`, `twilio`, `sendgrid`, `npm`, `private-key`,
`generic-secret`, `google`, `gcp-service-account`, `azure-storage`, `jwt`,
`high-entropy`.

## Use it

It runs by itself. Every message of every request — user, assistant and tool
result — is read on its way out, and each secret value becomes
`[REDACTED-SENSITIVE-VALUE]`; the system prompt is never touched. The log keeps
what was written, so `/sessions` and the transcripts still show the original,
and the request is the only place the placeholder stands. Nothing leaves the
machine: the detectors are local regular expressions, read against one string.

- `scan_secrets` — the tool: the model hands it text and gets the redacted
  text back with what was found, before it repeats a key anywhere.
- `/vaultknox` — what is being redacted and how many messages so far.
- `/vaultknox TEXT` — the same text as it would leave, with what was found.
- `(help :vaultknox)` — the same, for the model.

A secret in a message is a leak whether or not this cell saw it: rotate a key
that was pasted, and keep the new one out of chat.
