# nodecode-effortless

Effortless: judges a message's reasoning effort and sets it on the session. A Nodecode rewrite of [ivcained/effortless-hermes](https://github.com/ivcained/effortless-hermes) by [ivcained](https://github.com/ivcained).

Easy questions run cheap, hard jobs get deeper thinking. A fast judge reads the
next user message once — TypeSafe's Jev, a local OpenAI-compatible server, or
any custom API you name — and answers one rung of the ladder `low`, `medium`,
`high`, `xhigh`, `max`. The cell writes that rung to the session with
`nlk:select-effort`, so the turns from the next one on run at it.

The plugin did this automatically, as a middleware on every request. Nodecode
has the same knob as a session setting, so here the same judge runs on the
model's call, as the `pick_effort` tool, and on the operator's, as
`/effortless`.

## Get it

Tell the agent "add the effortless cell", or run `nodecode add nc://effortless`.

## Set it up

The default judge is TypeSafe's Jev, so it needs a key: put one in `/settings`
under the `effortless` section as `typesafe_key`, or set `TYPESAFE_API_KEY`.
A key is made at <https://console.typesafe.ai/>.

No key wanted? Point it at a model on your own machine:

```jsonc
{
  "effortless": {
    "judge_backend": "local",
    "local_base_url": "http://127.0.0.1:8080/v1",
    "local_model": "qwen3-4b"
  }
}
```

`custom` does the same against any OpenAI-compatible API: `custom_base_url`,
`custom_model`, `custom_api_key`. `fallback_backend` names the judge asked
when the first is unsure or down (`none` asks nobody else).

Bounding the verdict: `effort_floor`, `effort_ceiling` and `effort_bias`
(-2 cheaper .. +2 smarter) hold a judge inside what you want to pay for, and
`apply` (default true) is whether a verdict is written to the session at all.
A model whose own ladder lacks a rung still gets the nearest one below it.

## Use it

- The `pick_effort` tool. The model calls it when a message is plainly a big
  multi-step job or plainly a quick answer; it fills in the session itself and
  answers the rung it set and why.
- `/effortless` — the judge in force.
- `/effortless test` — one sample prompt through the judge chain.
- `/effortless stats` — the verdicts this run took, by rung and by judge.
- `/effortless <a message>` — judge that message and set the session on it.
- In eval: `(effortless:pick-effort "review the entire repo" :session-id "s-…")`.

A level named in the message ("max effort", "effort: high") wins with no
judge, and a short follow-up ("go", "ok") keeps the effort already in force.

What leaves the machine: the message (up to 4000 characters) and a short
excerpt of the conversation, to the judge you configured, to pick the rung.
Nothing else, and no telemetry.
