# nodecode-lightning-ai

Lightning AI Rate Limit: holds Lightning AI model calls to their per-minute limit. A Nodecode rewrite of [dinosauria123/hermes-lightningai-rate-limit](https://github.com/dinosauria123/hermes-lightningai-rate-limit) by [dinosauria123](https://github.com/dinosauria123).

## Get it

Tell the agent "add the lightning-ai cell", or run `nodecode add nc://lightning-ai`.

## Set it up

No key and no service: the cell counts the calls Nodecode already makes. Its section names the hosts to gate, and nothing else has to be set:

```jsonc
"lightning-ai": {
  "hosts": ["lightning.ai"],  // required: the endpoint hosts your Lightning AI key talks to
  "max_per_minute": 15,      // Lightning AI's own limit
  "window_seconds": 60       // the window the server counts in, aligned to the wall clock
}
```

## Use it

Lightning AI serves its API in a fixed wall-clock minute and allows about fifteen requests in one, with no `Retry-After` to honour. A burst of rounds — a tool loop, a fork, a few sessions at once — spends that in a few seconds and then collects 429s.

The cell counts the requests Nodecode sends to the hosts you name and makes the ones past `max_per_minute` wait for the next minute to begin, so the calls are spread over the window instead of refused. It sits on the one transport bracket every streaming lane shares, so a retried round is counted too. A provider that is not Lightning AI is never touched, and a failure in the gate lets the round run.

A 429 parks the rest of the window: the server has just said its counter is spent, so nothing goes out until the next minute.

`/lightning-ai` reports this window's state: how many of the limit are used, the seconds to the next, and how many calls were gated, waited or parked. `/lightning-ai reset` clears the counts, `/lightning-ai help` says the rest.

Everything stays in memory — counts and seconds, never a prompt or an answer. No network call of its own, no file, no process.
