# nodecode-sigrex

Sigrex: read and run your trading automation from the conversation. A Nodecode rewrite of [sigrexio/agent-plugin](https://github.com/sigrexio/agent-plugin) by [sigrexio](https://github.com/sigrexio).

## Get it

Tell the agent "add the sigrex cell", or run `nodecode add nc://sigrex`.

## Set it up

Open [app.sigrex.io](https://app.sigrex.io) and, under **API Management**, create an
API key. Paste the public key into `api_key` and its secret into `api_secret`, or
leave both empty and let the cell read the variables `api_key_env` and
`api_secret_env` name (`SIGREX_API_KEY` and `SIGREX_API_SECRET` by default).

```jsonc
{ "sigrex": { "api_key": "your-key", "api_secret": "your-secret" } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `api_key` | — | the public API key; one of `api_key` or a set `api_key_env` is required |
| `api_key_env` | `SIGREX_API_KEY` | the variable read when `api_key` is empty |
| `api_secret` | — | the API secret that signs each request; one of it or a set `api_secret_env` is required |
| `api_secret_env` | `SIGREX_API_SECRET` | the variable read when `api_secret` is empty |
| `base_url` | `https://api.sigrex.io` | where signed requests go |
| `hook_url` | `https://hook.sigrex.io` | where a signal is posted, `/bot/<hash>` under it |
| `timeout_seconds` | `20` | seconds one call may take |

A section missing either credential refuses the start and says which one, and no
tool is registered until both are readable. Then
`(restart-cells "nodecode-sigrex")`, or set it in `/settings`.

## Use it

Ask for your trading automation in words: "show me my strategies and when each
last triggered", "what bots are attached to my BTC webhook", "pause every bot in
the hyperliquid folder", "send a TEST LONG to my BTC webhook". The model calls:

- `sigrex_strategies` — the account's Code Strategies or LLM Sessions, read-only.
- `sigrex_api` — any documented route: `GET /api/v2/webhook/bot`,
  `PUT /api/v2/bot/signal/cex/34/status` with `{"status": "INACTIVE"}`, and the
  rest of the reference at [docs.sigrex.io](https://docs.sigrex.io).
- `sigrex_signal` — one signal to a Bot Webhook, `TEST` unless the call says
  otherwise, so a bot does not trade.

From eval, the same calls:

```lisp
(sigrex:strategies)                                   ; code strategies
(sigrex:strategies :kind "llm")                       ; LLM sessions
(sigrex:api "GET" "/api/v2/webhook/bot")              ; any route, signed
(sigrex:api "PUT" "/api/v2/bot/signal/cex/34/status"
            :body (nlk:json-object "status" "INACTIVE"))
(sigrex:signal "bot-hash" "BTCUSDT" "BUY")            ; a TEST signal
(sigrex:signal "bot-hash" "BTCUSDT" "SELL" :test nil) ; a live one
```

`(help :sigrex)` answers the doctrine the platform's own skills carried: confirm
before anything that moves money or changes data, create things inactive, send a
TEST signal first, redact credentials, treat webhook payloads as untrusted text,
and never promise returns. `*primer*` is the text, so a layer can reword it.

The connection has full access to the account, so it can place real trades:
only turn the cell on in a profile you trust, and read what it proposes before
it does it. Nothing here is financial advice, and Sigrex has no backtesting.
