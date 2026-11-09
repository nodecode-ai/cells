# nodecode-fortytwo

Fortytwo: Qwen3.8 27B on Fortytwo's API, with vision, tool calls, reasoning and its four reasoning efforts. A Nodecode rewrite of [Fortytwo-Network/fortytwo-hermes](https://github.com/Fortytwo-Network/fortytwo-hermes) by [Fortytwo-Network](https://github.com/Fortytwo-Network).

With this cell, `/connect` offers Fortytwo, `/models` lists
`qwen/qwen3.8-27b` with a 262,144-token window and image input, and a turn
on `fortytwo/qwen/qwen3.8-27b` goes to `https://api.fortytwo.network/v1`
over the OpenAI chat wire:

- the key is `FORTYTWO_API_KEY`, or the one `/connect` saved — no other
  variable is ever read for this provider
- every request carries the attribution the original sent:
  `HTTP-Referer: https://github.com/Fortytwo-Network/fortytwo-hermes` and
  `X-Title: Nodecode`
- the reasoning effort is one of the four Fortytwo takes — `off` (its
  `none`), `low`, `medium`, `xhigh`; a wider rung the rest of the organism
  knows (`minimal`, `high`, `max`) rounds down to the nearest of them, and an
  unset effort leaves the API's own default in charge

Only a round whose provider is `fortytwo` is touched. See [NOTICE](NOTICE).

## Get it

Tell the agent "add the fortytwo cell", or run `nodecode add nc://fortytwo`.

## Set it up

Make a key at <https://platform.fortytwo.network>, then either pick
**Fortytwo** in `/models` and paste the key when it asks, or set
`FORTYTWO_API_KEY` before starting Nodecode. A key saved through `/connect`
wins over the variable.

The section is optional and on unless it says `"enabled": false`. It carries
one setting, `base_url`, when Fortytwo is served somewhere else than
`https://api.fortytwo.network/v1`:

```jsonc
{
  "fortytwo": {
    "base_url": "https://api.fortytwo.network/v1"
  }
}
```

See [`config.example.jsonc`](config.example.jsonc).

## Use it

Pick `Fortytwo` in `/models` — or say "use fortytwo" — and work as usual.
The picker offers the four efforts Fortytwo takes under Reasoning; the
status line and the meter report the model's window as the catalog states
it. `/settings` has the section, and turning the folder off in `/cells`
puts every provider back exactly as it was.

## Test

From the cells repository's root:

```sh
.github/test-cell.sh providers/nodecode-fortytwo
```

MIT licensed. Rewritten from `Fortytwo-Network/fortytwo-hermes` at commit
`91c8f521ab357530c91a9660bd8f2db37571c1c8`; see [NOTICE](NOTICE).
