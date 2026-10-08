# nodecode-anthropic

A Nodecode cell that lets a Claude Pro or Max subscription serve the
`anthropic` provider. Sign in once with `/anthropic login`. When no Anthropic
key is set, `anthropic/<model>` rounds then run on the subscription.

The core already serves `anthropic` with an API key, and that path does not
change. A key in the config, a key `/connect` saved, `ANTHROPIC_API_KEY` or
`ANTHROPIC_AUTH_TOKEN` all still win over the sign-in. A key round goes out
exactly as it did before the cell was installed, with the same address,
headers and bytes.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `anthropic`
sign-in and the Claude Code transport its anthropic provider uses for a
subscription token. See [NOTICE](NOTICE).

## Before you use it

- **Anthropic's terms.** The Claude Agent SDK documentation says
  third-party products may not offer claude.ai login or plan rate limits
  without Anthropic's approval. This cell signs in to claude.ai with Claude
  Code's public client and makes each request look like Claude Code's. It
  is meant for running your own subscription on your own machine. Read
  Anthropic's current terms yourself before relying on it, and expect that
  Anthropic may refuse such requests or act on the account.
- **Billing.** Whether a round draws from your plan or from extra usage is
  decided by Anthropic's service. Turn off extra usage in your account if
  you don't want overage charges. Nodecode prices `anthropic` rounds at the
  API list price whatever the credential, so the Dashboard shows what a
  round would have cost on the API, not what the plan charged.
- **Long context.** The `context-1m` beta is never asked for, because a seat
  has no long-context credit. Models whose window is natively 1M serve it
  without the beta.
- **The grant lasts about 30 days.** Anthropic ends a sign-in's refresh
  tokens about a month after you sign in, however often they are refreshed.
  Then a round fails with a refresh error naming `/anthropic login`. Sign in
  again.

## Sign in

```
/anthropic login
```

The command answers with an address on `claude.ai`. Open it in a browser and
sign in. claude.ai sends the browser back to
`http://localhost:54545/callback`, or to another free port if 54545 is busy.
This cell listens there for five minutes, and a notice says when you are
signed in.

If the browser runs on another machine, claude.ai shows the code on its own
page. Copy it, along with the `#state` after it, and send:

```
/anthropic code CODE#STATE
```

The address the browser ended on works too.

The token is kept in Nodecode's shared `auth.json` under
`oauth_tokens.anthropic`:

```json
{"provider": "anthropic", "access_token": "sk-ant-oat01-...",
 "refresh_token": "...", "expires_at": 1791234567, "account_id": "...",
 "email": "...", "org_id": "...", "org_name": "...", "installation_id": "..."}
```

`expires_at` is in epoch seconds, saved five minutes early as omp saves it.
A token due within a minute of it is refreshed before a round sends it, and
the refreshed entry is written back. Every other field of `auth.json` is
kept, and the file stays mode 0600. When the token response does not say
whose account it is, the cell asks Claude Code's bootstrap endpoint.
`/anthropic status` says who is signed in and whether a key outranks the
sign-in. `/anthropic logout` takes the entry out.

## What a subscription round carries

Anthropic accepts a subscription token only on a request shaped like Claude
Code's own. A round on the `anthropic` lane whose credential is this cell's
sign-in, or whose key is a subscription token (`sk-ant-oat...`, as omp tells
them apart), goes out like this:

- `Authorization: Bearer` instead of `x-api-key`, to
  `/v1/messages?beta=true`
- Claude Code's headers: `User-Agent: claude-cli/2.1.280 (external, cli)`,
  `x-app: cli`, the Stainless SDK headers, `X-Claude-Code-Session-Id`, and
  Claude Code's beta set (`oauth-2025-04-20`, and `claude-code-20250219`
  for a request with tools or thinking)
- the system prompt opens with Claude Code's billing header (its version, a
  fingerprint of the first user message, and a `cch` attestation: the low 20
  bits of the body's XXH64) and its one-line identity, ahead of Nodecode's
  own prompt
- every tool name carries a `_` prefix, which is taken back off the answer's
  tool calls
- a `metadata.user_id` names the device, the session and the account
- cache marks live an hour, Claude Code's policy for a seat

The identity block carries no cache mark of its own. In omp it does, but
Nodecode already spends Anthropic's four cache marks per request, and the
mark on the last system block covers it.

## The catalog

The `anthropic` row gains omp's bundled models that models.dev does not list,
such as older Claude 3 models and the Mythos models. Nothing models.dev lists
changes, and no base is added, so the core's lane keeps its own address.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-anthropic/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Nothing to set. The section is on unless it says `"enabled": false`:

```jsonc
{
  "anthropic": {}
}
```

## Not ported

- omp's Foundry mode (`env hook="anthropic-foundry"`): enterprise gateway
  credentials, not a sign-in.
- Adopting a newer Claude Code version from a `claude_code_version_too_old`
  refusal. The version is pinned at 2.1.280 here. If Anthropic starts
  refusing it, the round fails with Anthropic's reason.
- Usage and quota reporting.
- omp's mid-conversation tool and effort controls, compaction and
  server-side fallback. The core builds the body, and this cell only reshapes
  it.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-anthropic
```

The sign-in's callback listens on a free loopback port the test dials. Every
Anthropic endpoint is a stubbed `dex:post` or `dex:get`, so nothing reaches
Anthropic. One test runs the same key round with and without the cell
started and compares the address, headers and body byte for byte.

MIT licensed.
