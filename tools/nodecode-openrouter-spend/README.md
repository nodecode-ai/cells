# nodecode-openrouter-spend

OpenRouter Spend: OpenRouter's own reported API-key spend, in USD and in INR, as a tool and a command. A Nodecode rewrite of [AllenRobinHubert/openrouter-spend](https://github.com/AllenRobinHubert/openrouter-spend) by [AllenRobinHubert](https://github.com/AllenRobinHubert).

One read-only GET to `openrouter.ai/api/v1/key` answers what OpenRouter's billing recorded for the key this home uses: today, this week, this month and all time, and what is left of the key's budget. The counters are the provider's own, so they cover every application using that key, not only this one. A public USD/INR reference rate, unless the operator pins one, puts the same amounts in rupees. It never calls a model, and no prompt, output or key is read or kept: the local record holds the counters and a fingerprint of the key.

## Get it

Tell the agent "add the openrouter-spend cell", or run `nodecode add nc://openrouter-spend`.

## Set it up

Export the OpenRouter key in `OPENROUTER_API_KEY`, or name the variable that holds it in the section's `key_env`. With no key set the cell refuses at start, and the refusal names the variable it looked for:

```jsonc
{
  "openrouter-spend": {
    "key_env": "OPENROUTER_API_KEY",
    "usd_inr_rate": "",
    "comparison_usd": "110",
    "comparison_label": "Codex"
  }
}
```

| Member | What it is |
| --- | --- |
| `key_env` | the variable holding the OpenRouter key, never the key itself (`OPENROUTER_API_KEY`) |
| `cache_seconds` | how long one reading answers before OpenRouter is asked again (30; 0 asks every time) |
| `timeout_seconds` | the ceiling on one accounting request (8) |
| `usd_inr_rate` | a manual INR per USD rate; empty reads the public daily reference rate |
| `fx_cache_seconds` | how long that rate is reused (43200, half a day) |
| `comparison_usd` | a monthly USD baseline the month is measured against (110, an illustrative one) |
| `comparison_label` | what that baseline is called in the one line about it (`Codex`) |
| `history_file` | where the readings are kept; empty is this home's `openrouter-spend/spend.json` |

## Use it

Ask the agent what your OpenRouter spend is, or type the command:

```
/openrouter-spend                     every period, the budget and the comparison
/openrouter-spend today               one period: today, week, month or total
/openrouter-spend refresh             ask OpenRouter again instead of the cache
/openrouter-spend ledger              the readings this home kept, and what was spent between them
/openrouter-spend status              the key in use, the record file, the cache settings
```

The same text is the `openrouter_spend` tool (`period`, `refresh`), and from eval:

```lisp
(openrouter-spend:report :period "month")
(openrouter-spend:report :refresh t)
(openrouter-spend:history)
```

A reading is kept each time the cell asks, so two of them show what the key spent in between. The first reading is the start of that record; OpenRouter's own counters are older, and `/openrouter-spend total` still reports them.

### What leaves the machine

The key goes in an `Authorization` header to `openrouter.ai/api/v1/key`, and nowhere else. The only other request is `api.frankfurter.dev`'s public USD/INR rate, which carries no key and no spend; pin `usd_inr_rate` and it is not asked at all. Nothing is posted, and the key is never written to the record: it is filed under a 64-bit fingerprint, so a ledger row says which key it belongs to without holding it.

### What it does not do

The original also kept a ledger of every request its host made -- cost, model, tokens, session -- through that host's hooks and execution middleware, and resolved missing costs from OpenRouter's generation ids. Nodecode's own counters are not those hooks, so the record here is one reading per fetch. See `NOTICE`.

## License

MIT. The original plugin is Copyright (c) 2026 Allen Robin Hubert: see `LICENSE` and `NOTICE`.
