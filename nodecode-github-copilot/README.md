# nodecode-github-copilot

A Nodecode cell for [GitHub Copilot](https://github.com/features/copilot),
signed in with a GitHub account. With it, `/models` lists Copilot's models,
`/github-copilot login` signs in, and a turn on `github-copilot/<model>` goes
to Copilot the way the Copilot CLI sends it:

- each model rides the wire Copilot serves it on, all at one address: Claude
  on Anthropic Messages (`/v1/messages`), the newer OpenAI, Grok and MAI
  models on the Responses API (`/responses`), the rest on OpenAI chat
  (`/chat/completions`); a model the bundled rows do not name is routed by
  omp's rules (`claude-*` on Messages, `gpt-5*`, `gpt-6*`, `grok-4.*`, `oswe*`,
  `mai-*` on Responses)
- every request carries the GitHub token as its bearer and the Copilot CLI's
  identity headers, says whether the operator or the loop after a tool sent
  it (`X-Initiator`, which decides whether Copilot bills a premium request),
  and flags a request that carries an image
- a personal account is served at the host GitHub names for its plan
  (`api.individual.githubcopilot.com` and the like); a GitHub Enterprise
  account at `copilot-api.<domain>`
- some Business organizations refuse the chat client identity and others the
  CLI's: a refused identity is tried once more as the other, and the one that
  worked is where the next round starts. `COPILOT_INTEGRATION_ID` pins one
- Messages rounds carry no Anthropic beta, which Copilot refuses
- `/models` asks the account's host for its chat models

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `github-copilot`
provider. See [NOTICE](NOTICE).

## Sign-in

```
/github-copilot login                  sign in at github.com
/github-copilot login acme.ghe.com     sign in at a GitHub Enterprise instance
/github-copilot status
/github-copilot logout
```

`login` answers at once with the page to open and the code to type there
(GitHub's device flow). Nodecode waits for GitHub in the background, keeps
the token, sets every bundled model's policy to enabled (Copilot serves some
models only after that), and says how it went. The token is kept
in `~/.nodecode/auth.json` under `oauth_tokens.github-copilot`:

```json
{"access_token": "gho_...", "refresh_token": "gho_...",
 "api_endpoint": "https://api.individual.githubcopilot.com", "enterprise_url": "acme.ghe.com"}
```

As in omp, the GitHub token is itself Copilot's bearer: no Copilot token is
exchanged for it. It does not expire on a clock, so it is kept with no
`expires_at`; an entry that carries one (omp stamps ten years out) is
refreshed when it comes due the way omp's hook refreshes it, the token kept,
without the network. A failed sign-in stands as a notice until the next one
succeeds; a success is said once.

Without a sign-in, `COPILOT_GITHUB_TOKEN` is read: a GitHub token, sent the
same way, whose plan host is asked of GitHub once per process. A key saved
with `/connect` (a GitHub token, or omp's `{"token", "enterpriseUrl",
"apiEndpoint"}`) answers before either. No other provider's key is ever sent
to Copilot.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-github-copilot/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "github-copilot": {
    // where Copilot is served; a relay's address goes here
    "base_url": "https://api.githubcopilot.com"
  }
}
```

A plan or Enterprise host is used only while `base_url` is Copilot's own
host; a relay configured here stays the address. A model's wire can be
pinned in the operator's config (`providers.github-copilot.models[].sdk`),
which outranks the cell's routing.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-github-copilot
```

## Gaps

Carried without a core change, with these left out:

- Copilot's long-context siblings (`<model>-1m`) and its tiered prices, which
  omp synthesizes from the listing's `billing.token_prices`: the listing
  answers ids and windows only.
- Premium-request accounting: `X-Initiator` is sent as omp sends it, but the
  round's usage keeps no premium-request count (the core's PROVIDER-USAGE
  has no slot for one).
- The per-class compat rules of `providers/github-copilot.kdl` reach the
  organism only as each bundled row's effort ladder; a discovered model the
  rows do not carry runs on the core's defaults for its wire.

MIT licensed.
