# nodecode-kilo

A Nodecode cell for [Kilo Gateway](https://kilo.ai), a gateway that serves
some six hundred models from many vendors behind one account. With it,
`/kilo login` signs you in with a device code, `/connect` offers Kilo
Gateway for a key, `/models` lists its models under their vendors' ids
(`anthropic/claude-opus-4.7`, `qwen/qwen3.7-max`, ...), and a turn on
`kilo/<model>` goes to `https://api.kilo.ai/api/gateway` over the OpenAI chat
wire, with one quirk carried from omp: a Qwen model is told whether to think
with `enable_thinking`, the Qwen dialect, instead of `reasoning_effort`.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `kilo`
provider. See [NOTICE](NOTICE).

## Sign in

```
/kilo login
```

omp's Kilo login is a custom one: a device flow against Kilo's own
device-auth endpoint. The command asks Kilo for a code and answers with the
page Kilo names and the code. Open the page on any device, approve the code,
and this machine notices within five seconds (it asks every five seconds
until Kilo's code expires). A notice says when you are signed in or why the
sign-in failed: denied, expired, timed out. Nothing listens on this machine.

The token goes into Nodecode's shared `auth.json` under `oauth_tokens.kilo`:

```json
{"provider": "kilo", "access_token": "...", "refresh_token": "", "expires_at": 1791234567}
```

Kilo hands back a bearer with no refresh token, and omp keeps it for a year.
So does this cell. After that, a turn is refused with "sign in again with
/kilo login", and that notice stands until you do. Every other field of
`auth.json` is kept, and the file stays mode 0600. `/kilo status` says
whether you are signed in, and `/kilo logout` takes the entry out.

## Key

A key works as well. Save it with `/connect`, or set `KILO_API_KEY`. The
order: a key `/connect` saved (the core reads it before any cell is asked),
then the sign-in, then `KILO_API_KEY`, as omp puts a stored credential
before the environment. With none of them, nothing is sent as a key. The
chat family's `OPENAI_API_KEY` never reaches Kilo.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-kilo/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "kilo": {
    // where Kilo Gateway is served; a relay's address goes here
    "base_url": "https://api.kilo.ai/api/gateway"
  }
}
```

## Thinking

omp resolves each model's thinking dialect from its identity class. The Qwen
class (every id naming `qwen` or `qwq`, and `prism-ml/ternary-bonsai-2-27b`)
speaks the Qwen dialect: for a reasoning model, an effort sends
`enable_thinking: true` and no `reasoning_effort`, and `off` sends
`enable_thinking: false`. With no effort set, nothing is sent, so the
provider's default applies, as the chat lane does for every provider. Every
other model keeps the chat lane's `reasoning_effort`.

## Not ported

- Per-model replay rules omp keeps for a few models (`thinking-requires-effort`,
  reasoning content on every assistant turn for some DeepSeek ids). The chat
  lane already replays `reasoning_content` once a model streamed one, and
  treats every DeepSeek id as a thinking model.
- Live model discovery. The core lists any credentialed catalog provider
  from `<base>/models`, which is what omp's Kilo discovery does.

## Gaps

- **Per-model idle deadline.** omp gives `moonshotai/kimi-k2.6` a 300 s
  stream-idle timeout. A round's idle deadline is the frozen
  `EFFECTIVE-PROVIDER-CONFIG-REQUEST-TIMEOUT` (from
  `providers.kilo.stream_idle_timeout_ms` in the operator's config, for the
  whole provider), and `NLE::WALK-PROVIDER-STREAM` takes no keyword that
  would set it for one round. The seam would be a `:request-timeout` keyword
  on `WALK-PROVIDER-STREAM`, or a per-model rung in
  `SNAPSHOT-EFFECTIVE-PROVIDER-CONFIG`.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-kilo
```

Kilo's device endpoints and every round are stubbed `dex:post` and `dex:get`
calls, and the wait between two polls is stubbed out, so nothing reaches
Kilo and nothing sleeps.

MIT licensed.
