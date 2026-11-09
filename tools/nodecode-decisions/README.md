# nodecode-decisions

Decisions: ask yes/no questions about a text and get, for each, the probability that it is true, from an OpenAI-shaped Decisions endpoint. A Nodecode rewrite of [eve-ai-dev/hermes-decisions-plugin](https://github.com/eve-ai-dev/hermes-decisions-plugin) by [eve-ai-dev](https://github.com/eve-ai-dev).

A model reads sentences that are not quite empty — a ticket that may be urgent, a paragraph that may be a complaint, a plan that may be out of scope — and asks a decision endpoint for a number instead of an opinion. Two tools do it, and the answer is the endpoint's own JSON, extra members included.

## Get it

Tell the agent "add the decisions cell", or run `nodecode add nc://nodecode-decisions`.

## Set it up

A key is required: it goes in the `decisions` section of `~/.nodecode/config.jsonc`, either written as `api_key` (kept out of the file, in `~/.nodecode/secrets/`) or named by `api_key_env`, the variable that holds it — `OPENAI_API_KEY` by default.

```jsonc
{
  "decisions": {
    "api_key_env": "OPENAI_API_KEY",
    "endpoint": "https://api.openai.com/v1/decisions",  // any absolute http(s) URL, no query
    "model": "gpt-6-luna",                              // whatever the endpoint accepts
    "timeout_seconds": 120                              // 1..600; a call past it is not retried
  }
}
```

An endpoint of your own needs `endpoint` and `model` changed together; the key goes to that endpoint and nowhere else. Without a key the cell still starts, and each call says which setting fills it. `/settings` and the Settings tab draw the same five fields, and `(restart-cells "nodecode-decisions")` picks up an edit without a relaunch.

## Use it

While the cell runs, `(help :decisions)` teaches both tools, and every request carries one line naming them.

- `decisions_submit` takes `input` (the text, at most 256 KiB with its questions) and `questions`, one to 64 of them, each `{"type": "predicate", "instructions": "is it urgent?"}`. Ask a question in the affirmative: the number is the probability that it is true.
- `decisions_status` takes `job_id` and answers that submission of this session: `running`, or `completed` with the reply object, or `failed` with the reason.

`background` defaults to true, as in the original: the call answers a job id at once and leaves the request running on a thread of its own, so a slow endpoint never holds a turn. `background: false` waits for the probabilities and answers them, and the job is still there afterwards to read back. `/decisions` lists this session's submissions and what became of them, and `/decisions <job_id>` answers one job's whole record.

An answer looks like `{"type": "predicate", "name": null, "probability": 0.87}`. Four submissions run at once and 32 finished ones are kept, all in memory: a restart keeps none, and a job belongs to the session that started it. Nothing is retried — a failed call says whether the outcome is uncertain, and asking again is a second call.

The cell refuses what the endpoint cannot mean: a reply with a different number of answers than questions asked, a probability outside 0..1, a non-predicate or named answer, a body over 1 MiB, and a URL with a scheme, query or fragment it cannot POST to.

## Tests

```sh
.github/test-cell.sh tools/nodecode-decisions
```

Offline: the one network call is bound to a scripted reply and a background job runs on the test's own thread, so the request body, the job record, the session's ownership of a job and each refusal are read as facts. Eleven tests, including the cell's start and stop, both tools, the slash command, and a section with no key.
