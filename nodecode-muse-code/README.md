# nodecode-muse-code

A Nodecode cell for Muse Code, Meta's Muse subscription. With it,
`/muse-code login` signs in to a Meta account with a device code and mints
the Model API key the subscription authorizes, `/models` lists the Muse
Spark models, and a turn on `muse-code/<model>` goes to
`https://api.meta.ai/v1/responses` the way oh-my-pi sends it:

- the minted key is the bearer, never the Meta account token, with
  `x-api-version: 1.0.0` beside it
- `tool_choice` is never sent: Meta's endpoint refuses every form but auto
- the picker's live listing asks `GET /v1/models` the way oh-my-pi's
  discovery does, with the minted key and the API version; its models join
  the bundled rows
- results are not stored on Meta's side (`store: false`), which is
  oh-my-pi's default too

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `muse-code`
provider. See [NOTICE](NOTICE).

## Sign-in

```
/muse-code login     answers at once with the address to open and the code to enter
/muse-code status    signed in, and as whom
/muse-code logout    forgets the sign-in
```

The device code is asked of `https://auth.meta.com/oidc` with Muse Code's
client. The login polls on a background thread; the account token that
arrives is spent once on `https://api.meta.ai/muse-code/key`, which onboards
the account and answers the key, and the outcome comes as a notice: signed
in, the subscription is inactive, or a subscription is required (with the
address to take one). The sign-in is kept in the shared `auth.json` under
`oauth_tokens.muse-code` (`access_token`, the account token; `api_key`, the
minted key; `account_id`; `email`). Meta's device answer names no expiry and
its token endpoint refuses the refresh grant, so there is no `expires_at`
and nothing is refreshed: an account whose subscription lapses signs in
again.

A Meta Model API key saved with `/connect` also works, on the same wire. No
environment variable is read, and the Responses lane's own fallback never
sends `OPENAI_API_KEY` to Meta.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-muse-code/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "muse-code": {
    // where Meta's Model API is served
    "base_url": "https://api.meta.ai/v1"
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-muse-code
```

## Not ported

- The subscription usage report (the key endpoint's `subs_usage`).
- oh-my-pi's compact edit-prompt variant, which is its own tool prompt.

## Gaps

- Stored responses and stream resume. oh-my-pi can opt in to `store: true`
  on Meta (it finishes a run after the client's socket dies, and
  `GET /responses/{id}` returns it), and resumes a dropped stream from
  there. Nodecode's Responses lane (`call-responses-streaming`) always sends
  `store: false` and re-runs a cut stream; a cell could set `store` through
  `responses-request-body`, but resuming needs a core seam: a hook the
  Responses lane calls when its stream is cut, taking the config and the
  response id the stream announced (`response.created`) and answering the
  finished response to fold, instead of re-running the request.
- An authoritative listing. oh-my-pi treats Meta's live listing as the
  roster (`dynamic-models-authoritative`): a model the account cannot use
  leaves the picker. Nodecode's picker (`model-choices`) shows the catalog's
  rows and the listing's together, so a listing can add a model but never
  take a bundled one away. The seam would be a catalog row member (say
  `listing_authoritative`) that makes the picker show only the listing's
  rows for that provider once a listing has answered.

MIT licensed.
