# nodecode-openrouter-everything

OpenRouter Everything: OpenRouter's decisions, extractions, reranking, embeddings and web search on one OpenRouter key. A Nodecode rewrite of [dadhalfdev/openrouter-everything](https://github.com/dadhalfdev/openrouter-everything) by [dadhalfdev](https://github.com/dadhalfdev).

The plugin wired five of OpenRouter's non-chat APIs into Hermes: System One
decisions, structured extraction, reranking, embeddings and the
`openrouter:web_search` server tool. This cell keeps those five calls and the
model-per-capability idea behind them, and drops the parts that were Hermes
plumbing (it rewrote Hermes' `config.yaml` to become the web, image, video,
speech and transcription provider; a Nodecode provider lane is another cell's
job).

## Get it

Tell the agent "add the openrouter-everything cell", or run:

```sh
nodecode add nc://openrouter-everything
```

## Set it up

You need an OpenRouter key, from <https://openrouter.ai/settings/keys>. The
cell reads it from any of:

- `api_key` in the `openrouter-everything` section of the config (pasted in `/settings`, kept in a 0600 file);
- the variable `api_key_env` names;
- `OPENROUTER_API_KEY`, read either way.

Every other member is optional: `base_url` for a relay of your own (the
default is `https://openrouter.ai/api/v1`), one `*_model` per capability when
the plugin's curated default is not what you want, and `timeout_seconds`.

```jsonc
{
  "openrouter-everything": {
    "api_key_env": "OPENROUTER_API_KEY",
    "timeout_seconds": 60
  }
}
```

`/openrouter-everything` says what the cell is connected to — the base,
whether a key is set, and the model each verb would use — without ever
printing the key.

## Use it

Five verbs in the `openrouter-everything` package, called through eval, and
five tools of the same names for the model:

| Verb | What it answers |
| --- | --- |
| `(openrouter-everything:decide STATE :options MAP)` | calibrated probabilities over candidate choices, in one pass |
| `(openrouter-everything:extract-fields STATE FIELDS)` | typed fields with spans and confidence |
| `(openrouter-everything:rerank QUERY DOCUMENTS :top-n N)` | documents ranked by relevance, best first |
| `(openrouter-everything:embed TEXTS)` | vectors, dimensions, and every pair's cosine similarity |
| `(openrouter-everything:search QUERY :limit 5)` | a synthesis plus the urls it read |

`decide` and `extract-fields` live under OpenRouter's `/api/alpha` root, the
other three under `/api/v1`; both are derived from `base_url`. Every call
bills the key, and every refusal — no key, a bad base, a refused status — is
an `OPENROUTER-EVERYTHING-ERROR` naming what to fix. `(help
:openrouter-everything)` carries the same manual the model reads.

The shipped `websearch` cell searches with no key at all; reach for this
cell's `search` when you want OpenRouter's own engine behind the same key, and
for the other four when one answer beats a conversation.

## Test it

```sh
.github/test-cell.sh tools/nodecode-openrouter-everything
```

Every request is a stubbed `nlk:http`: the tests run offline.
