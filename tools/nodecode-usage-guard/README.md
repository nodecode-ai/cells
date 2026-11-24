# nodecode-usage-guard

Usage Guard: reads the subscription usage windows behind a Claude or ChatGPT/Codex plan and says when one is spent, so a long run stops before it spends paid extra usage. A Nodecode rewrite of [mister-ideas/usage-guard](https://github.com/mister-ideas/usage-guard) by [mister-ideas](https://github.com/mister-ideas).

## Get it

Tell the agent "add the usage guard cell", or run `nodecode add nc://usage-guard`.

## Set it up

Name what to watch, and nothing else: the guard reads the sign-in Claude Code and Codex already hold, and sends that token to its own provider's usage endpoint only.

```jsonc
{
  "usage-guard": {
    "sources": ["claude", "codex"]
  }
}
```

- `sources` is required and is what the cell watches: `claude` reads Claude Code's login, `codex` reads the Codex login. A source with no sign-in on this machine answers `cannot confirm`.
- `threshold_percent` defaults to 100, the subscription's own wall; 97 leaves a margin.
- `warn_percents` defaults to `[80, 90]`: the levels at which one note rides the next turn.
- `min_interval_s` (180) and `max_age_s` (60) keep the endpoints' small hourly budget intact: a reading is reused while it is young, and never re-read more often than the floor.
- `fail_open` (false) decides what a missing or stale reading means: unconfirmed, or fine.
- A token may come from a variable instead of the app's file: `claude_token_env` / `codex_token_env` name it.

## Use it

Ask the model how much of the subscription is left, and it calls `usage_guard`:

```
usage-guard OK
  claude: 5 hour 12.5% (resets in 3h 40min), 7 day 3.0% (resets in 5d 2h) (read 22s ago)
  codex: primary 41.0% (resets in 2h 10min) (read 22s ago)
Every watched window has room left.  (claude, codex; at the limit from 100% used)
```

The first line is the verdict: `OK`, `BLOCKED` (a window is at the threshold) or `UNVERIFIED` (no fresh reading, and `fail_open` is off). `/usage-guard` prints the same table, `/usage-guard check` asks for a read now, and `(usage-guard:report)` answers it from eval.

Once a window passes a warn level, every turn's first round is told once, so a long run winds down by itself:

> [usage-guard] claude at 92.0% -- a subscription window is over 90% used. Wind the current thread down and wait for the reset before starting anything long.

Nodecode has no pre-call veto, so the guard is the reading and the warning, not a refusal: the model reads the verdict and stops, or you switch model with `/model`.

## What is different from the original

The original gates Hermes's request path: a request to a provider whose window is spent is refused before it is sent. Nodecode has no such middleware point for a cell, so this rewrite keeps the reading, the cache discipline and the warning, and turns the hard refusal into the verdict's first line (see [NOTICE](NOTICE)).

## Tests

Offline, in a scratch home, against a Nodecode source tree; every HTTP call is stubbed:

```sh
NODECODE_SRC=/path/to/nodecode .github/test-cell.sh tools/nodecode-usage-guard
```

15 tests, 0 failed -- install and stop, the tool through its registration, the reading and its cache, a blocked window, a lower threshold, an unconfirmed source, a failed refresh keeping the last good reading, the read floor, the Codex answer, the command, and the note in a turn's first round.

## License

MIT, as the original. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
