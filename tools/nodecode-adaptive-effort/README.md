# nodecode-adaptive-effort

Adaptive Effort: a scorer model judges each task and picks this session's reasoning effort. A Nodecode rewrite of [atostivint/hermes-adaptive-effort](https://github.com/atostivint/hermes-adaptive-effort) by [atostivint](https://github.com/atostivint).

A conversation is a mix of one-line lookups and hard debugging. Hermes's plugin
asked a separate scorer - TypeSafe's Jev by default - how much reasoning the
task in front of the model deserved, then set the request's effort to match.
This cell asks the same question of the same service and lands the answer
through Nodecode's own effort pick, the one `/think` and the footer's rung use,
so the model that answers and the rung it runs at stay in step.

## Get it

Tell the agent "add the adaptive-effort cell", or run `nodecode add nc://adaptive-effort`.

## Set it up

Make a key at <https://console.typesafe.ai/> and put it in the environment:

```sh
export TYPESAFE_API_KEY=...
```

`/settings` holds the rest, and every member is optional:

```jsonc
{
  "adaptive-effort": {
    "mode": "off",              // off | once | always
    "scorer": "jev",            // jev | custom
    "endpoint": "",             // empty: TYPESAFE_BASE_URL, else https://api.typesafe.ai
    "model": "",                // empty: TYPESAFE_DEFAULT_MODEL, else jev-latest
    "custom_endpoint": "",      // scorer "custom": the full URL of a System One endpoint
    "custom_auth": "none",      // none | bearer (reads CUSTOM_SCORER_API_KEY)
    "timeout_seconds": 3,
    "prompt_chars": 4000,
    "instructions": "",         // extra guidance for the scorer
    "max_turns": 32,
    "notify": true
  }
}
```

A System One endpoint of your own - a local DJev server, say - speaks the same
route: set `"scorer": "custom"`, `"custom_endpoint": "http://127.0.0.1:8011/v1/systemone"`,
`"model": "dgemma"`, and `"custom_auth": "none"` to send no key. Nothing is
sent anywhere but that endpoint.

## Use it

The cell starts **off**: nothing is scored and no effort moves until you ask.

```lisp
(aeffort:probe "rename parse-config's argument")   ; the judgment alone
(aeffort:choose "rewrite the scheduler")            ; judge, and land the rung
(aeffort:history)                                   ; what this process decided
(aeffort:status)                                    ; mode, scorer, key, last pick
```

`/adaptive` says the same from the composer: `/adaptive status`,
`/adaptive history`, `/adaptive probe TASK`, `/adaptive choose TASK`.

With `"mode": "always"` the cell decides at every new turn's first round, and
with `"once"` once per session, judging the newest thing you typed; the pick
lands as a notice and in the ledger. A pick applies **from the next turn** (a
turn binds its effort when it starts), as any reasoning-effort pick does, and -
the way `/think` works - it also becomes the default new sessions start on.

The scorer sees the task text, cut at `prompt_chars`, and nothing else: no
history, no tool output, no file contents. A classification that fails - no
key, a timeout, a status, an answer without a score - leaves the effort
exactly as it was.

## What it is not

- Not every scorer of the original: OpenAI Decisions, OpenRouter Decisions,
  Cloudflare Clef and Nace Drex each need their own wire; this cell speaks
  TypeSafe's System One, the default, and any endpoint that answers it.
- No named-choice question for a route's own rungs, no dialogue context, no
  continuation hysteresis, no tool-error escalation, no cache-aware dynamic
  trial, and no cross-restart history: the label is always the plugin's three
  level rubric, clamped onto the rungs the target admits.
- No retry and no pause: one request, one deadline. A failed call fails open.

MIT licensed; see [LICENSE](LICENSE) and [NOTICE](NOTICE).
