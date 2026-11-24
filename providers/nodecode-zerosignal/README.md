# nodecode-zerosignal

ZeroSignal: private, pay-per-request inference through the local zs-proxy, with a funded wallet as the credential. A Nodecode rewrite of [TxnLab/hermes-zerosignal-provider](https://github.com/TxnLab/hermes-zerosignal-provider) by [TxnLab](https://github.com/TxnLab).

ZeroSignal is a pay-per-request, privacy-preserving inference network reached
through `zs-proxy`, a proxy on the same machine that speaks the OpenAI chat
wire at `http://127.0.0.1:9376/v1`. With this cell, `/connect` offers
ZeroSignal, `/models` lists the models the proxy currently serves, and a turn
goes to the proxy:

- the request's `reasoning_effort` is clamped **down** onto the levels the
  serving node declares in the proxy's own `/v1/models` (`reasoning.supported`,
  `reasoning.allowed_efforts`) — never upward, so a node that declares
  `low/high/max` is never sent `medium`
- a model that declares `reasoning.supported: false` is sent no reasoning
  parameter at all, and a model the proxy has not named passes through as it is
- no key is needed: any non-empty value works, because the proxy ignores the
  `Authorization` header and the wallet pays

## Get it

Tell the agent "add the zerosignal cell", or run `nodecode add nc://zerosignal`.

## Set it up

Start and fund the proxy first on the same machine:

```sh
zs-proxy proxy start
zs-proxy fund
```

See <https://docs.zerosignal.ai/using-the-proxy/guides/hermes> for installing
`zs-proxy`.

Then pick ZeroSignal in `/models`. The picker asks for a key; any non-empty
value does (`zerosignal-local` is the one the cell sends when none is saved),
and `ZEROSIGNAL_API_KEY` in the environment works the same. The proxy's
address is optional, for a proxy on another port:

```jsonc
{
  "zerosignal": {
    "base_url": "http://127.0.0.1:9376/v1"
  }
}
```

## Use it

Ask for a model — `/models` lists what the proxy serves, and the curved
list in this folder's fallback is what it shows before the proxy answers —
then talk as usual. `/think` offers the rungs the chosen model declares, and a
pick outside them is clamped before it reaches the network.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-zerosignal
```

MIT licensed. See [NOTICE](NOTICE).
