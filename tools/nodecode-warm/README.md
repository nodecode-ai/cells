# nodecode-warm

Prompt Cache Warmer: keeps a provider's prompt cache warm between turns, by sending an idle session's own request back one token at a time. A Nodecode rewrite of [mojomast/hermes-session-warmer](https://github.com/mojomast/hermes-session-warmer) by [mojomast](https://github.com/mojomast).

Every OpenAI-compatible endpoint that caches a prompt charges less for the part of the context it has already seen, and the cache expires after a few quiet minutes. This cell keeps it alive: when a turn ends, Nodecode keeps the exact body that turn sent; once the session has been idle for the interval, that body goes back once with `max_tokens: 1` and no tools, so the next question still finds its prefix cached and cheaper. Nothing is invented, nothing is rewritten, and the reply's text is read and dropped — only numeric usage is kept.

## Get it

Tell the agent "add the warm cell", or run:

```sh
nodecode add nc://warm
```

## Set it up

Two settings, both required, and the cell refuses to start until they are there:

| Setting | What it is |
|---|---|
| `base_url` | the OpenAI-compatible endpoint whose cache is kept warm — normally the base URL of the provider you already use, such as `https://api.example.com/v1` |
| `models` | the model ids it may warm; a session sending any other model is left alone |

There is one entry per line in `/settings` (the `warm` section of `~/.nodecode/config.jsonc`), or the whole section at once:

```jsonc
{
  "warm": {
    "base_url": "https://api.example.com/v1",
    "models": ["some-model"],
    "api_key_env": "MY_PROVIDER_KEY",
    "interval_seconds": 240
  }
}
```

`base_url` must be HTTPS, or plain HTTP on loopback; the cell refuses anything else, because a conversation is what travels. `api_key_env` names the environment variable holding that endpoint's key — the only credential the cell ever reads — and a local endpoint that needs no key may leave it out.

| Setting | Default | What it does |
|---|---|---|
| `interval_seconds` | `240` | how long a session is idle before its request goes again (30–3600) |
| `idle_window_seconds` | `1800` | how long a session keeps being warmed after its last turn |
| `max_requests` | `7` | how many times one idle session is re-sent, at most |
| `timeout_seconds` | `15` | one warming request's ceiling |
| `max_sessions` | `4` | how many sessions are kept warm at once |
| `max_payload_bytes` | `524288` | a request larger than this is not warmed |

Every re-send is a request your provider bills. A one-token request against a cached prefix is the cheapest request there is, and `max_requests` caps what one idle stretch can cost, but the bill is real: the cell is off until you add it, and `/warm stop` silences it for the rest of the process.

## Use it

Ask a question, read, come back ten minutes later: the context is still cached. `/warm` answers what it has done —

```
warm: 3 sent (3 answered, 0 failed), 0 skipped, 1 session idle · cached 3072 of 3600 prompt tokens
      · https://api.example.com/v1, models some-model · every 240s, at most 7 per session in 1800s
```

— and `/warm stop` ends the warming for this process. `(help :warm)` carries the same explanation into the agent's manual.

What the cell never does: send a model the section does not list, a request larger than the cap, or anything at all while a session is taking turns; keep the reply's text (the answer is read for its `usage` object and dropped); touch a credential store (the key is whatever `api_key_env` names); retry a failed request (a failure drops that session's warming until its next turn).

## Tests

```sh
.github/test-cell.sh tools/nodecode-warm
```

No network: the session's kept body is a literal and the transport is a stub, so the suite proves the lifecycle, that a finished turn's request goes back with one token and no tools, that another model is skipped, that a stop silences the tick, and that a plain-HTTP or keyless-of-endpoint configuration is refused by name.

## Credits

A rewrite of mojomast's Hermes Agent plugin, MIT, at commit `eba7be111c44e4ac6b702cdcb341c8d71da1c57e`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
