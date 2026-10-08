# nodecode-zai-coding-plan

A Nodecode cell for [Z.AI](https://z.ai)'s GLM Coding Plan, signed in from
the browser. With it, `/zai-coding-plan login` signs the plan in, `/models`
lists GLM-5.3 and its kin, and a turn goes to Z.AI the way oh-my-pi sends it:

- every GLM model omp knows rides the Anthropic Messages endpoint at
  `https://api.z.ai/api/anthropic`, the key sent as a bearer, thinking on a
  budget (`budget_tokens`, and for GLM-5.2 and GLM-5.3 the effort beside it)
- GLM-5.3-Flash, which that endpoint does not carry, rides the native chat
  endpoint at `https://api.z.ai/api/coding/paas/v4`, with Z.AI's
  `thinking` switch beside `reasoning_effort`
- a model only models.dev lists for `zai-coding-plan` keeps riding the chat
  endpoint, as it did before this cell

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`zai-coding-plan` sign-in and `zai` provider. See [NOTICE](NOTICE).

## Sign in

```
/zai-coding-plan login
```

answers at once with the address to open. It is ZCode's own sign-in: Z.AI
allows this client only ZCode's desktop redirect, `zcode://zai-auth/callback`,
so when the browser lands there (or offers to open ZCode), copy that whole
address, or the code in it, and paste it:

```
/zai-coding-plan code zcode://zai-auth/callback?code=...&state=...
```

The sign-in finishes in the background and says how it went as a notice. It
exchanges the code for a short-lived token, logs in to Z.AI's business API
with it, finds or creates a key named `nodecode` in the account's default
project, and keeps `<apiKey>.<secretKey>` in auth.json under
`oauth_tokens.zai-coding-plan`. That key does not expire, so nothing is
refreshed. `/zai-coding-plan status` says whether the plan is signed in,
`/zai-coding-plan logout` forgets the key (it stays live at Z.AI until you
delete it there).

Without the sign-in: make a key at <https://z.ai/manage-apikey/apikey-list>
and save it with `/connect`, or set `ZAI_API_KEY` (or `ZAI_CODING_PLAN_API_KEY`).
A key saved with `/connect` wins over the sign-in, the sign-in over the
variables. No other provider's key is ever sent to Z.AI: with none of these
the round goes keyless and Z.AI says why.

omp's environment overrides work here too: `ZAI_OAUTH_CLIENT_ID`,
`ZAI_OAUTH_AUTHORIZE_URL`, `ZAI_OAUTH_REDIRECT_URI`, `ZAI_OAUTH_TOKEN_URL`,
`ZAI_BIZ_BASE`, `ZAI_BUSINESS_LOGIN_URL`.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-zai-coding-plan/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "zai-coding-plan": {
    // the chat endpoint (GLM-5.3-Flash, and what only models.dev lists)
    "base_url": "https://api.z.ai/api/coding/paas/v4",
    // the Messages endpoint; the lane appends /messages
    "anthropic_base_url": "https://api.z.ai/api/anthropic/v1"
  }
}
```

A `providers.zai-coding-plan` entry in the config that names an `sdk` or a
`base_url` outranks both: the operator's declaration wins over omp's route.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-zai-coding-plan
```

## Not ported

- omp's native scheme receiver: on a supported desktop omp registers itself
  for `zcode://` for the length of the sign-in, so the redirect comes back on
  its own. Here the address is pasted.
- omp replays a GLM model's unsigned thinking as thinking blocks
  (`replayUnsignedThinking`); the core's Messages lane drops unsigned
  thinking from the history it sends.

MIT licensed.
