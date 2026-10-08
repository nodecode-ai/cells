# nodecode-xai-oauth

A Nodecode cell for xAI Grok on a SuperGrok or X Premium+ subscription.
With it, `/xai-oauth login` signs in to xAI with a device code, `/models`
lists the Grok models the subscription serves (Grok Build, Grok 4.3 to 4.7,
the Grok 4.20 SKUs, Grok Composer 2.5 Fast), and a turn on
`xai-oauth/<model>` goes to `https://api.x.ai/v1/responses` the way
oh-my-pi sends it:

- the access token is the bearer, on the same paid Responses API an xAI key
  rides
- `reasoning.effort` is left out for the models that refuse it (Grok Build,
  the `-reasoning` SKUs); elsewhere minimal is sent as low, and xhigh and max
  as high except on Grok 4.6, 4.7 and 4.20 Multi-Agent, which take xhigh
- `reasoning.summary` is never sent, and encrypted reasoning is asked for on
  every reasoning model so the next round replays it
- a tool schema whose root is a union of bare required-key fragments loses
  that union, which xAI refuses (the object stays)
- the session rides `x-grok-conv-id`, which xAI's prompt cache routes on

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `xai-oauth`
provider, whose sign-in oh-my-pi adapted from NousResearch/hermes-agent.
See [NOTICE](NOTICE).

## Sign-in

```
/xai-oauth login     answers at once with the address to open and the code to enter
/xai-oauth status    signed in, as whom, and when the token expires
/xai-oauth logout    forgets the sign-in
```

The device code is asked of `https://auth.x.ai` with the Grok CLI's client
and scopes. The token endpoint is read from xAI's OIDC discovery document at
every login and refresh, and refused unless it is https on `x.ai` or a
subdomain, since every refresh token is sent there. The login polls on a
background thread and says the outcome as a notice; the userinfo endpoint
then names the account's email. The token is kept in the shared `auth.json`
under `oauth_tokens.xai-oauth` (`access_token`, `refresh_token`,
`expires_at` in epoch seconds, `account_id`, `email`) and refreshed before
a round when it expires within a minute.

Without a sign-in, a token set in `XAI_OAUTH_TOKEN`, or a key saved with
`/connect`, is used. `XAI_API_KEY` is not read here (oh-my-pi's dedicated
mode), and the Responses lane's own fallback never sends `OPENAI_API_KEY`
to xAI.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-xai-oauth/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "xai-oauth": {
    // where xAI is served
    "base_url": "https://api.x.ai/v1"
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-xai-oauth
```

## Not ported

- Grok Imagine and Grok TTS, which ride image and speech wires, not a chat
  lane (tools/omp-models.py keeps them out).
- oh-my-pi's `XAI_BASE_URL` override, which never applies to an OAuth token
  anyway; `base_url` in the section is the knob here.
- The SuperGrok credit usage report (`cli-chat-proxy.grok.com/v1/billing`)
  and oh-my-pi's live model discovery, which merges `/v1/models` over the
  curated rows.

## Gaps

None found: every rule above rides an existing core seam.

MIT licensed.
