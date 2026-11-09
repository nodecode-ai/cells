# nodecode-skill-router

TypeSafe Skill Router: names the one kept skill that fits the request, before the model call. A Nodecode rewrite of [DECRUX9812/typesafe-skill-router](https://github.com/DECRUX9812/typesafe-skill-router) by [DECRUX9812](https://github.com/DECRUX9812).

Nodecode shows the model one line about every definition it keeps. With a
few hundred of them the model reads past the one that would have done the
job. This cell sends the request to [TypeSafe](https://typesafe.ai) first
and, when a skill genuinely fits, adds one line to the round about to be
asked:

```
<skill_relevance>
Relevant to the current request: queue-records. Ignore this if it does not fit what the user
actually asked for.
</skill_relevance>
```

It injects **nothing** when nothing fits, and every failure path — no key,
no roster, a timeout, a transport error — stays silent: a routing problem
can never break a turn.

## Get it

Tell the agent "add the skill-router cell", or run `nodecode add nc://skill-router`.

## Set it up

Make a key at <https://console.typesafe.ai/> and put it where this home keeps
secrets — `TYPESAFE_API_KEY` in the environment of the process, or the same
variable named in the section. The section lives in
`~/.nodecode/config.jsonc`; every member is optional:

```jsonc
{
  "skill-router": {
    "api_key_env": "TYPESAFE_API_KEY",
    "base_url": "",          // empty: https://api.typesafe.ai
    "model": "",             // empty: jev-latest
    "gate": "0.30",          // mean of the three request judgments; below it, nothing is named
    "fits": "0.40",          // the winner's own fit judgment; below it, nothing is injected
    "shortlist": 3,
    "chunk": 240,            // skills per request; the API caps one choice at 255 options
    "excerpt": 700,          // characters of a candidate's own text stage two shows
    "timeout": 10,
    "suggest_chars": 4000    // longer requests are left alone
  }
}
```

A `base_url` of your own speaking the same `POST /v1/systemone` takes no
key: with none configured the request goes without an `Authorization`
header. With no key and no base of your own the cell still starts and says
nothing.

## Use it

Nothing to type. Put the cell in and each turn's first round is routed:

- the roster is the skills this image keeps — what `(index)` lists, including
  skills another harness keeps and a folder links in. Memories are not
  routed; only `define-skill` definitions are.
- the request is that turn's own committed text. Slash commands, empty
  requests and anything longer than `suggest_chars` are left alone.
- the answer is at most one name, and the line rides as that round's
  reminder — never the system prompt, so the cached prefix stays put.

`/skill-router` answers status: where the key comes from, how many skills are
kept, the endpoint and the two thresholds. `/skill-router <text>` routes one
request now and says what it named, without asking a model — the way to try a
threshold against real requests before a turn spends anything:

```
/skill-router queue the new record on the kitchen speaker
queue-records — queue-records fits 0.88 (gate 0.78)
```

`(help :skill-router)` carries the same manual into the conversation.

## How it works

Two requests, thresholds in the section:

1. **Wide.** One `choice` per chunk of `chunk` skills over the same
   one-line descriptions the index shows, each chunk also offering
   `none_of_these`; plus three `noul` judgments on the request (does it act
   on the user's own material, does it follow a documented procedure, would
   prose alone satisfy it). The gate is the mean of those three, the third
   inverted.
2. **Narrow.** One `choice` over the top `shortlist` plus `none_of_these`,
   and one `noul` per candidate: does this skill do the specific thing the
   request asks for?

A chunk whose `P(none_of_these)` is at least a half nominates nobody, except
the best chunk, so a shortlist is never empty when something fits.
Candidates rank by probability then name, so a replay is bit-stable.

## Test

From the repository root:

```sh
.github/test-cell.sh tools/nodecode-skill-router
```

The slice is offline: every exchange is a stubbed POST answering canned JSON,
so the chunking, the two thresholds and the recorded line are exercised with
no key and no network.

## What the rewrite does not carry

The original's on-disk answer cache and its per-turn log file, and its
`on`/`off`/`status`/`suggest`/`check` command family: a section is the
switch here, and the answers are cheap enough to ask again. The thread-pool
budget around a decision is gone too — the HTTP call takes a timeout of its
own. See [NOTICE](NOTICE).

MIT — see [LICENSE](LICENSE).
