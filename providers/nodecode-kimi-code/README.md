# nodecode-kimi-code

A Nodecode cell for [Kimi Code](https://www.kimi.com/code), Moonshot's
coding subscription. With it, `/kimi-code login` signs in with a device
code, `/models` lists the Kimi Code models (K3, K2.8 Preview as
`kimi-for-coding`, K2.7 Code Highspeed, K2.5, K2), and a turn on
`kimi-code/<model>` goes to `https://api.kimi.com/coding/v1/messages` the
way oh-my-pi sends it:

- every bundled model rides Kimi's Anthropic-compatible Messages wire
  (oh-my-pi declares `kimiApiFormat: anthropic` for each of them), with the
  credential as a bearer and no `x-api-key`
- every request, the sign-in's included, carries the Kimi CLI's fingerprint
  headers (`User-Agent: KimiCLI/<version>`, `X-Msh-Platform`, `X-Msh-Version`,
  and the machine's name, model, OS version and a device id kept in the
  home's `kimi-device-id`)
- K3, K3-256k and K2.8 Preview think adaptively (`thinking: {type:
  adaptive}` and the effort in `output_config`); off on K3 asks for its
  lowest rung, since that endpoint refuses thinking off, and off on K2.8
  Preview pins the effort to low
- a thinking request keeps every replayed thinking block
  (`context_management` with `clear_thinking ... keep: all`), without which
  the backend strips the reasoning the history replays
- the picker's live listing asks `GET /coding/v1/models` the way oh-my-pi's
  discovery does (a bearer, `User-Agent: KimiCLI/1.0`, `X-Msh-Platform`);
  its models join the bundled rows

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `kimi-code`
provider. See [NOTICE](NOTICE).

## Sign-in

```
/kimi-code login     answers at once with the address to open and the code to enter
/kimi-code status    signed in, as whom, and when the token expires
/kimi-code logout    forgets the sign-in
```

The login polls Kimi's token endpoint on a background thread and says the
outcome as a notice. The token is kept in the shared `auth.json` under
`oauth_tokens.kimi-code` (`access_token`, `refresh_token`, `expires_at` in
epoch seconds, `account_id` from the token's `user_id` claim) and refreshed
before a round when it expires within a minute. `KIMI_CODE_OAUTH_HOST` or
`KIMI_OAUTH_HOST` moves the sign-in to another host, as in oh-my-pi.

Without a sign-in, a key saved with `/connect` or set in `KIMI_API_KEY`
(else `KIMI_CODE_API_KEY`) is used. The cell never lets the Messages lane's
own fallback send `ANTHROPIC_API_KEY` to Kimi.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-kimi-code/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "kimi-code": {
    // where Kimi Code is served; a relay's address goes here
    "base_url": "https://api.kimi.com/coding/v1"
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-kimi-code
```

## Not ported

- The wire a listed model names. oh-my-pi's discovery reads each listed
  model's `protocol` and sends one that names none over the OpenAI chat
  wire; every bundled model names the Messages wire, and this cell sends
  every model, listed or bundled, there.
- oh-my-pi's per-provider in-flight limit, thinking-loop guard and leaked
  thinking-markup healing, which are its runtime's, not the wire's.

## Gaps

None found: every rule above rides an existing core seam.

MIT licensed.
