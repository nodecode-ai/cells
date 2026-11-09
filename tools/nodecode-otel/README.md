# nodecode-otel

OpenTelemetry for a Nodecode session: every tool call, model round and failed turn becomes a span, posted as OTLP/HTTP JSON to the collector you name — or kept in one local file when you name none. A Nodecode rewrite of [briancaffey/hermes-otel](https://github.com/briancaffey/hermes-otel) by [briancaffey](https://github.com/briancaffey).

## Get it

Tell the agent "add the otel cell", or run `nodecode add nc://otel`.

## Set it up

Nothing is required: without an endpoint every span stays in `~/.nodecode/otel/spans.jsonl` and nothing leaves the machine.

To export, name one OTLP/HTTP traces URL in the `otel` section of `~/.nodecode/config.jsonc` (or `/settings`, or the page's Settings tab):

```jsonc
{
  "otel": {
    "endpoint": "http://localhost:6006/v1/traces",
    "headers": ["x-api-key: ${PHOENIX_API_KEY}"]
  }
}
```

| Collector | `endpoint` |
| --- | --- |
| Phoenix | `http://localhost:6006/v1/traces` |
| Jaeger | `http://localhost:4318/v1/traces` |
| Grafana Alloy / LGTM | `http://localhost:4318/v1/traces` |
| Langfuse | `http://localhost:3000/api/public/otel/v1/traces`, with `headers: ["authorization: Basic <base64 public:secret>"]` |
| any OTLP/HTTP collector | its `/v1/traces` |

A secret stays out of the file: `headers` reads `${VAR}` from the environment, and `api_key_env` names a variable holding a bearer token that goes in `authorization`.

How much text rides along is `content_capture`: `full` sends whole tool arguments and results, `preview` (the default) the first `preview_chars` of each, `off` none at all. `api_key_env`, `service_name`, `local_store`, `max_spans`, `flush_seconds` and `timeout_seconds` are the rest — see `config.example.jsonc`.

## Use it

- `/otel` — what is exported where, how much has been recorded and sent, and how the last export went; `/otel spans 20`, `/otel send`, `/otel clear` for the local file.
- `otel_status`, `otel_spans` (newest first, filter by a word in the name) and `otel_mark` — the last one lets the model put a marker span on the trace at a milestone, so a person reading the spans later finds the moment.
- Every tool call is a `CLIENT` span named `tool <name>` with `gen_ai.tool.name`, the arguments and result previews, the result size and its status; a failed call is an error span.
- Every completed model round is `chat <provider>/<model>` with the time to the first token and the stream's length, and each round's usage is `usage <provider>/<model>` with `gen_ai.usage.input_tokens`, `output_tokens`, `cached_input_tokens`, the finish reason and the cost. A failed turn is an error span carrying the reason.
- All spans of one turn share a trace id, so a collector shows the round, its tool calls and its spend together.

The local file is `~/.nodecode/otel/spans.jsonl`, newest `max_spans` kept.

## What leaves the machine

Only to the endpoint you name, and only over that one connection: nothing is discovered, fetched or updated. Read `/otel` before you trust the quiet.

## License

Apache-2.0, as the original: see `LICENSE` and `NOTICE`.
