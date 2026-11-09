# nodecode-cacheguard

Cacheguard: warns or blocks a big prompt sent after a pause, when the provider's prompt cache may have expired, and shows what the round reused or rebilled. A Nodecode rewrite of [AIverflow/hermes-cacheguard](https://github.com/AIverflow/hermes-cacheguard) by [AIverflow](https://github.com/AIverflow).

## Get it

Tell the agent "add the cacheguard cell", or run `nodecode add nc://cacheguard`.

## Set it up

No key and no service: the cell reads only what Nodecode already tells it. Its section is optional and everything has a default:

```jsonc
"cacheguard": {
  "mode": "warn",              // off, warn (report afterwards) or block (stop before sending)
  "pause_minutes": 10,         // idle minutes that count as a pause
  "size_k": 20,                // smallest prompt to act on, in thousands of tokens
  "display_on_success": false, // show the line when the cache was kept
  "journal_rows": 10000        // most calls kept in the stats
}
```

## Use it

Providers bill a cache hit far below their input price, and after a pause the provider may have dropped the cache: the next big prompt is then billed at full price, or more where writing a new cache costs extra. cacheguard keeps, per session, when the last provider call ran and how large its prompt was, and wraps the one function that makes a provider call:

- **warn** (the default): the round runs, and afterwards a notice says `cacheguard: cache lost, 48k rebilled, 12m pause`, or `cacheguard: 41k reused, 12m pause` when `display_on_success` is on. A kept cache is silent otherwise.
- **block**: on a pause over `pause_minutes` and a prompt over `size_k` tokens, the provider is not called at all. The round answers `cacheguard: BLOCKED, 12m pause, 48k tokens may be rebilled.`; sending the message again goes out, because the pause clock restarted. Starting a new chat is often cheaper still, and the new chat can read the old session back by its id.
- **off**: it watches nothing.

`/cacheguard` shows the settings and a table of how often the cache was lost, by how long the pause was, from this image's own journal. `/cacheguard set mode block`, `/cacheguard set pause_minutes 5`, `/cacheguard reset`, `/cacheguard help`.

The cell never calls a model, never opens a socket and starts no process. It stores counts and minutes, never message text, and a failure in it lets the round run.
