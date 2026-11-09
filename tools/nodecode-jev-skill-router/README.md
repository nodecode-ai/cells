# nodecode-jev-skill-router

Jev Skill Router: names at most one kept skill that fits a request, before anything is loaded. A Nodecode rewrite of [ydmw74/jev-skill-router](https://github.com/ydmw74/jev-skill-router) by [ydmw74](https://github.com/ydmw74).

Nodecode shows the model one line about every definition it keeps, cut at 60
characters. With a hundred of them the two skills that look alike — the
`.pptx` *editing* one and the *authoring* one — read the same, and on a turn
where nothing fits the roster still invites a load. This cell answers the
question the index cannot:

```
user request ──▶ skill_select ──▶ Jev (TypeSafe's Decisions API)
                                  1. rank the whole roster, and gate the turn
                                  2. rerank the top 3 with their own text
                 ◀── one skill name, or an abstain
```

It answers **one** name or abstains, and an abstain is a real answer: on a
prose question the model keeps its own judgement and loads nothing.

## Get it

Tell the agent "add the jev-skill-router cell", or run `nodecode add nc://jev-skill-router`.

## Set it up

Two backends, one payload. Either make a key at
<https://console.typesafe.ai/keys> and let `JEV_TYPESAFE_API_KEY` hold it, or
point the cell at an endpoint of your own that forwards `{state, questions}` to
`api.typesafe.ai/v1/systemone` and returns the raw answer, with its bearer
token in `JEV_SKILL_ADAPTER_TOKEN`. An adapter, when one is named, wins.

The section lives in `~/.nodecode/config.jsonc`; every member is optional:

```jsonc
{
  "jev-skill-router": {
    "api_key_env": "JEV_TYPESAFE_API_KEY",
    "adapter_url": "",                   // an endpoint of your own; wins over base_url
    "adapter_token_env": "JEV_SKILL_ADAPTER_TOKEN",
    "base_url": "",                      // empty: https://api.typesafe.ai
    "model": "",                         // empty: jev-latest
    "gate": "0.30",                      // mean of the three request judgments
    "fits": "0.30",                      // the shortlist's best fit judgment
    "shortlist": 3,
    "excerpt": 700,                      // characters of a candidate's own text
    "timeout": 60
  }
}
```

With no key and no token the cell still starts: a call answers what to set and
spends nothing.

## Use it

The model calls one tool:

```
skill_select {request: "the user's request, as they typed it"}
→ {decision: "suggest" | "abstain", suggested: "queue-records" | null,
   roster_size, gate, gate_values, shortlist, fits,
   suggestion_block, seconds}
```

`suggestion_block` is the `<skill_relevance>` paragraph to put after the
roster, so the roster itself stays byte-stable and prefix caching holds.
`gate_values` and `fits` say why the decision came out as it did.

The roster is every skill this image keeps — the same definitions `(index)`
lists, `(help 'name)` reads, including skills another harness keeps and a
`SKILL.md` a folder links in.

`/jev-skill-router` answers status: where the credential comes from, how many
skills are kept, the endpoint and the thresholds. `/jev-skill-router <text>`
routes one request now and says what it named — the way to try a threshold
against real requests before a turn asks a model.

`(help :jev-skill-router)` carries the same manual into the conversation.

## How it works

Two calls, thresholds in the section:

1. **Rank and gate.** One `choice` over every skill name, on the same
   one-line descriptions the index shows, plus three judgments on the request:
   does it act on the user's own material, does it follow a documented
   procedure, and (inverted) would prose alone satisfy it. The gate is the
   mean of the three; below `gate` nothing is suggested.
2. **Rerank.** The top `shortlist` candidates come back with their full
   description and `excerpt` characters of their own text, one
   does-this-skill-do-the-specific-thing judgment each, and one `choice` with
   a `none_of_these` option. Nothing is suggested when the best judgment is
   below `fits`.

The answer is at most one name. Candidates rank by probability then name, so a
replay is stable.

## Test

From the repository root:

```sh
.github/test-cell.sh tools/nodecode-jev-skill-router
```

The slice is offline: every exchange is a stubbed POST answering canned JSON,
so both calls, both thresholds and the answer object are exercised with no key
and no network.

## What the rewrite does not carry

The stdio MCP server loop and its `uv` launcher — Nodecode dispatches the tool
itself, so the loop had nothing left to do. The per-session roster cache is
gone too: the roster is read per call, which is what a live index is. The
companion `SKILL.md` is this README and `(help :jev-skill-router)`. See
[NOTICE](NOTICE).

MIT — see [LICENSE](LICENSE).
