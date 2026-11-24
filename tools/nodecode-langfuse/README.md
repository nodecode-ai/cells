# nodecode-langfuse

Langfuse: traces each turn, model call and tool call to your own Langfuse project. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

## Get it

Tell the agent "add the langfuse cell", or run `nodecode add nc://nodecode-langfuse`.

## Set it up

Two keys, from the Langfuse project's **Settings → API keys**, go in the cell's section in `~/.nodecode/config.jsonc` (or in `/settings`, where a pasted secret is kept in a 0600 file under `~/.nodecode/secrets/`):

```jsonc
{
  "langfuse": {
    "public_key": "pk-lf-...",
    "secret_key": "sk-lf-...",
    "base_url": "https://cloud.langfuse.com"
  }
}
```

`base_url` defaults to Langfuse Cloud; a self-hosted instance puts its own address there. Then `/langfuse test` sends one test trace: it answers `a test trace went to …` when the keys work, and the server's refusal when they do not.

| setting | default | what it does |
| --- | --- | --- |
| `public_key` | *required* | the project's public key (`pk-lf-...`) |
| `secret_key` | *required* | the project's secret key (`sk-lf-...`) |
| `base_url` | `https://cloud.langfuse.com` | your Langfuse instance |
| `capture` | `sanitized` | `sanitized`, `metadata` or `full` — see below |
| `max_chars` | `12000` | the ceiling on any one content field |
| `max_depth` | `4` | how deep nested tool arguments are captured |
| `environment` | | a tag on every trace, for example `production` |
| `release` | | a tag on every trace, for example `v1.2.0` (also sent as the trace's `release`) |

## Use it

There is nothing to call. Once the cell is on, every turn is traced:

- one **trace** per turn, named `Nodecode turn`, with the Nodecode `sessionId` as its Langfuse session, and metadata carrying the turn's rounds, tool calls, seconds, status and capture mode;
- one **generation** per provider round, with the provider, the model, the token counts the provider reported (input, output, cache read, cache write, reasoning, total) and the cost the core already priced for that round, as `costDetails`;
- one **span** per tool call, with the call's arguments as input, its result as output, its wall time, and `ERROR` when the call failed.

The batch goes to `<base_url>/api/public/ingestion` when the turn ends (`turn.completed`, `turn.failed` or `turn.cancelled`). A backend that is slow or down never holds a turn up: the batch is posted from the frame the fact already publishes, and if the server refuses it the batch stays held and one standing notice says why. `/langfuse send` retries everything held; `/langfuse show` prints the next batch exactly as it would go; `/langfuse clear` forgets what is held.

`(help :langfuse)` in the manual says the same, and `(langfuse:status)` answers the one line `/langfuse` opens with.

### Capture modes

Content is *prompts, results and tool arguments*; identity, roles, token counts, cost and timing are always sent.

| mode | what leaves |
| --- | --- |
| `metadata` | no content: each content field becomes `{"omitted": true, "type": "text", "chars": N}` |
| `sanitized` **(default)** | content with token-looking words (`sk-…`, `pk-…`, `ghp_…`, `Bearer …`) and values assigned to secret-named keys (`password=…`) replaced by `<redacted>`, **then** cut at `max_chars` |
| `full` | raw content, cut at `max_chars` only |

`sanitized` is pattern-based defense in depth, not a DLP guarantee: for a shared Langfuse project, prefer `metadata`.

### Where this differs from the original

The original hooked Hermes's `pre_api_request` / `post_api_request` / `pre_tool_call` / `post_tool_call` / `subagent_*` points and sent through the `langfuse` Python SDK. Nodecode has no such points; it has the `:frame` point, which every durable fact passes through, and the `:tool` around hook. So this cell reads the same facts the core already recorded — `turn.usage` (provider, model, the eight token counts, the priced cost) and the turn's terminal fact — and posts the same shapes over Langfuse's plain HTTP ingestion API, one batch per turn. What that leaves out is written in [NOTICE](NOTICE).

## Test it

```sh
.github/test-cell.sh tools/nodecode-langfuse
```

The tests are offline: the one seam that leaves, `nodecode-langfuse::*post*`, is stubbed, so a run proves the batch's shape and never dials a Langfuse.
