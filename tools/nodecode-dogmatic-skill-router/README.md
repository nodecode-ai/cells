# nodecode-dogmatic-skill-router

DoGMaTiiC's Skill Router: names the one skill of yours that fits the turn, before the model call. A Nodecode rewrite of [DoGMaTiiC/hermes-jev](https://github.com/DoGMaTiiC/hermes-jev) by [DoGMaTiiC](https://github.com/DoGMaTiiC).

You keep procedures as prose skills — `(define-skill name "one sentence" ;; the steps ...)`. Past a handful of them the index is a list, and the model has to guess which one this turn wanted. This cell asks a judgment model first: one choice over your whole roster, a second look at the best three, two thresholds, and at most one name back. The turn then carries one line:

```
<harness key="skill_relevance">
Relevant to the current request: xlsx. Load it with (help 'xlsx) when it fits; ignore this if it does not fit what the user actually asked for.
</harness>
```

It says nothing when nothing fits, and the line tells the model to ignore it when it is wrong: the model stays in charge.

## Get it

Tell the agent "add the dogmatic-skill-router cell", or run `nodecode add nc://dogmatic-skill-router`.

## Set it up

It needs one key, from any of three routes — TypeSafe's own API, OpenRouter's Decisions API, or the Vercel AI Gateway:

| Route | Key | Where it is read |
| --- | --- | --- |
| TypeSafe (default) | `TYPESAFE_API_KEY` | the environment, or `api_keys.typesafe` in `auth.json` |
| OpenRouter | `OPENROUTER_API_KEY` | the environment, or `api_keys.openrouter` in `auth.json` |
| Vercel AI Gateway | `AI_GATEWAY_API_KEY` | the environment |

Make a TypeSafe key at <https://console.typesafe.ai/>. Set it in the cell's section or export it; `backend` picks a route, and `auto` takes the first that has a key. With no key it loads, starts and stays silent — `/dogmatic-skill-router check` says which key it is missing.

Everything else is optional and documented in the section: `mode` (`auto` by default: speak when a key is present; `on`; `off`), `gate` and `fits` (the two thresholds, in percent), `shortlist`, `excerpt`, `suggest_chars`, `timeout`, and the model and base URL of each route.

## Use it

- Just talk. Every prompt is routed before its turn reads its context, and the turn is given one skill name at most.
- `/dogmatic-skill-router` — where it stands: mode, backend, key, roster size, thresholds; its panel lists the roster, runs a live suggestion, and checks the key.
- `/dogmatic-skill-router skills` — the roster it routes over.
- `/dogmatic-skill-router suggest <text>` — one decision on that text, now.
- From eval, `(dogmatic-skill-router:route "text")` answers the same line, and `(dogmatic-skill-router:skills)` lists the roster.

## What it does

Two judgment calls per eligible turn, and none when the turn is left alone:

1. **Rank.** One choice question over every skill you keep, each option its one-sentence description, plus three yes/no questions about the turn (does it act on your machine, does it want a documented procedure, would prose do — the last inverted). Their mean is the gate.
2. **Re-read.** The best three come back with their own text, 700 characters each, and one "does it fit" question each. Below `gate`, or below `fits`, nothing is named.

Rosters past the API's 255-option cap are chunked at 240, each chunk with a `none_of_these` option. A slash command, an empty message, a message longer than `suggest_chars`, and a turn that already carries a line are left alone.

It is advisory by construction: no key, no roster, a slow API, a wrong answer — every path ends in silence, and three failures in a row open a breaker for two minutes so a turn never waits on a router that is down.

## What it does not do

- It does not load the skill, or run it: it names one, and `(help 'name)` reads it.
- It does not rank the cells you have installed, only the prose skills you keep.
- It does not log decisions to a file: `/dogmatic-skill-router` and `(dogmatic-skill-router:route ...)` answer with what it decided.
