# nodecode-understudy

Understudy: auditions cheaper models on your own past decisions and reports how often each could have taken yours. A Nodecode rewrite of [stffinfcti/understudy](https://github.com/stffinfcti/understudy) by [stffinfcti](https://github.com/stffinfcti).

## Get it

Tell the agent "add the understudy cell", or run `nodecode add nc://understudy`.

## Set it up

The cell needs an OpenAI-compatible endpoint and one model to judge with:

- `base_url` — `https://openrouter.ai/api/v1` unless you name another.
- `api_key` or `api_key_env` — the endpoint's key (`OPENROUTER_API_KEY` is
  read either way).
- `judge_model` — the model id that compares a candidate's answer with the
  one you actually got. It is required: a folder without it refuses to start
  and says so.

Optional: `sample` (past decisions per candidate, 8), `days` (how far back
to look, 14), `budget_usd` (a hard cap for one run, `"1.50"`), and `prices`
(`"model=input/output"`, USD per 1M tokens) so the scorecard can say what a
run cost. `/understudy` shows what the cell is connected to.

## Use it

Ask the model to audition a model, or type it yourself:

```
/understudy moonshotai/kimi-k3 openai/gpt-6-mini
/understudy last
/understudy runs
```

The cell reads your recent turns from this home's session history, replays a
sample of the questions on each candidate, has the judge compare each answer
with the one the agent actually gave, and answers a scorecard: the takeover
rate with its sample size, the misses, the tokens, and the cost when the
endpoint or `prices` says it. It costs real tokens: one call per decision per
candidate, plus one judging call each, stopped at `budget_usd`.

The model reaches the same two verbs as `understudy_audition` and
`understudy_report`, and `(help :understudy)` is the primer it reads.

## What the port leaves out

The original was written against Hermes's own model facade and its session
database, drew a PNG and an HTML report, and priced models from a local
models.dev cache. This cell speaks one OpenAI-compatible endpoint over
`nlk:http`, replays questions and answers (not full tool-call transcripts),
and answers a text scorecard. See NOTICE.
