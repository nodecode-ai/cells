# nodecode-xai-oauth

A Nodecode cell for xAI Grok on a SuperGrok or X Premium+ subscription.
With it, `/xai-oauth` signs in to xAI with a device code, `/models`
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
/xai-oauth → Sign in   answers at once with the address to open and the code to enter
/xai-oauth             signed in, as whom, and when the token expires
/xai-oauth → Sign out  forgets the sign-in
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

## Grok Imagine: the `xai_oauth_image` tool

With the cell running, the model has one more tool, `xai_oauth_image`:

```
xai_oauth_image {"prompt": "a red fox in snow", "output": "fox.png",
                 "aspect_ratio": "1:1", "resolution": "1k"}
```

- `prompt` and `output` are required; `output` is a new leaf filename inside
  `media_root` ending `.png`, `.jpg`, `.jpeg` or `.webp`. Paths, `..`, other
  extensions and unknown arguments (an `api_key` among them) are refused.
- `aspect_ratio` is one of xAI's ratios (`1:1` by default, as oh-my-pi
  sends it; `16:9`, `9:16`, `4:3`, `3:4`, `3:2`, `2:3`, `2:1`, `1:2`,
  `19.5:9`, `9:19.5`, `20:9`, `9:20`, `21:9`, `5:2`, `auto`); `resolution`
  is `1k` (default), `1.5k` or `2k`.
- One request: `POST {base_url}/images/generations` with model
  `image_model` (`grok-imagine-image` by default), `n` 1 and
  `response_format` `b64_json`. The bearer is the stored sign-in's access
  token, refreshed first when it expires within a minute. Nothing else asks
  in its place: not `XAI_OAUTH_TOKEN`, not a `/connect` key, not a configured
  API key. The token is only sent when `base_url` is https on `x.ai`.
- xAI's bytes are written as they came, never re-encoded. The file's
  extension follows the format the bytes are (sniffed from their magic
  number), so `fox.png` asked and a JPEG answered lands as `fox.jpg`. xAI
  usually answers JPEG.
- Never overwrites: if the name, or its stem under any of the four
  extensions, already exists (a dangling link counts), the call is refused
  before anything is sent. The write is a temp file in `media_root`, then a
  hard link that itself refuses an existing name; nothing is left behind
  when a call fails.
- The answer is JSON: `path`, `mime_type` (sniffed), `provider_mime_type`
  (what xAI said), `requested`, `bytes`, `model`, `revised_prompt`, and
  `usage`, `rate_limit` and `limits` (see below).

### Limits

The tool says what xAI reports and nothing more. `usage` is xAI's usage
object verbatim (`cost_in_usd_ticks` when xAI sends it), `rate_limit` holds
the `x-ratelimit-*` response headers verbatim, and `limits` puts both into
one sentence, saying "xAI reported no usage" or "no rate-limit headers"
when that is what came back. xAI reports no image quota or remaining-images
figure for a subscription sign-in, so the tool never shows one. Whether
image requests count against a SuperGrok credit is xAI's to say, and this
cell does not claim it.

### Failure modes

| What happened | What the call answers |
|---|---|
| no sign-in stored | refusal naming `/xai-oauth login`; nothing sent |
| sign-in expired, refresh refused | the refresh's credential error; nothing sent |
| output name taken (any of the four extensions, links too) | `... already exists in media_root`; nothing sent |
| bad name, ratio, resolution or argument | refusal naming it; nothing sent |
| name longer than a filesystem holds (stem over 250 bytes) | `... is too long`; nothing sent |
| the filesystem refuses the write for another reason | `could not write <name> in media_root: <the system's words>`; no file |
| `base_url` not https on `x.ai` | refusal; the token is not sent |
| xAI refuses (plan, policy, rate limit) | `xAI refused the image request: HTTP N: <xAI's words>`; no retry, no other credential, no file |
| xAI answers without `b64_json`, or not JSON | `no image bytes`; no file |
| the bytes are not PNG, JPEG or WebP | `not a PNG, JPEG or WebP`; no file |
| no answer within 180 s | `... did not answer within 180 s`; no file |

Image edits (`/v1/images/edits`), several images per call and xAI's
deferred or stored outputs are not ported.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-xai-oauth/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "xai-oauth": {
    // where xAI is served
    "base_url": "https://api.x.ai/v1",
    // existing directory xai_oauth_image writes into (leaf filenames only)
    "media_root": ".",
    // the Grok Imagine model xai_oauth_image asks
    "image_model": "grok-imagine-image"
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-xai-oauth
```

## Not ported

- Grok TTS, which rides a speech wire, not a chat lane (tools/omp-models.py
  keeps it out). Grok Imagine is ported as a tool, above, not as a model.
- oh-my-pi's `XAI_BASE_URL` override, which never applies to an OAuth token
  anyway; `base_url` in the section is the knob here.
- The SuperGrok credit usage report (`cli-chat-proxy.grok.com/v1/billing`)
  and oh-my-pi's live model discovery, which merges `/v1/models` over the
  curated rows.

## Gaps

None found: every rule above rides an existing core seam.

MIT licensed.
