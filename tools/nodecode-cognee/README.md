# nodecode-cognee

Cognee memory: recall, remember and forget a knowledge graph on a local or remote Cognee server. A Nodecode rewrite of [topoteretes/cognee-integrations](https://github.com/topoteretes/cognee-integrations) by [topoteretes](https://github.com/topoteretes).

The model gets four tools — `cognee_recall`, `cognee_remember`,
`cognee_forget`, `cognee_switch_dataset` — that keep what a user stores in a
Cognee knowledge graph and find it again by meaning, and `/cognee` to show
the server or recall from the shell.

## Get it

Tell the agent "add the cognee cell", or run `nodecode add nc://cognee`.

## Set it up

The cell talks to a cognee server: the one its own package starts on
`127.0.0.1:8011`, or a remote deployment. That address is the `cognee`
section's `base_url`. A server on this machine needs no key; a remote one
needs one, in `COGNEE_API_KEY` or the section:

```jsonc
{
  "cognee": {
    "base_url": "https://cognee.example.com",
    "api_key_env": "COGNEE_API_KEY",   // or "api_key" inline
    "dataset": "nodecode"              // whose memories these are
  }
}
```

A remote `base_url` with no key is refused at the start, naming the member,
as is a value under its floor (`top_k`, the two timeouts).

| Member | Default | What it does |
| --- | --- | --- |
| `base_url` | `http://127.0.0.1:8011` | the Cognee API address |
| `api_key` | unset | the key a remote server wants |
| `api_key_env` | `COGNEE_API_KEY` | the variable holding it |
| `dataset` | `nodecode` | the dataset these memories live in |
| `search_type` | blank | the SearchType every recall asks for, e.g. `CHUNKS`; blank lets Cognee route the query |
| `top_k` | `10` | passages one recall asks for unless the call says otherwise |
| `recall_timeout_seconds` | `120` | a recall's ceiling: `GRAPH_COMPLETION` runs an LLM per query |
| `write_timeout_seconds` | `120` | a remember, forget or dataset lookup's ceiling |

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that the release branch is cut on Fridays.

The model calls `cognee_remember`, which stores one document in the
dataset and lets Cognee's pipeline cognify it server-side. Later, "when do we
cut the release branch?" makes it call `cognee_recall`, which answers the
passages nearest the question. `/cognee` prints the server and the dataset;
`/cognee when do we cut the release` recalls from the shell.

Forgetting is two-phase, as in the original: `cognee_forget` with
`action="find"` and terms lists candidate documents with previews of their
stored text, the model shows them to the user, and `action="forget"` with
the confirmed `data_ids` and `confirm=true` deletes exactly those. Clearing a
whole dataset needs `everything_in_dataset` and an explicit user request.

`cognee_switch_dataset` lists the datasets the key sees, reports the current
one, moves this conversation to another, or returns it to the configured
default.

## The wire

`POST /api/v1/recall` (JSON: `query`, `top_k`, `datasets`, `scope`
`["graph"]`, `search_type`), `POST /api/v1/remember` (multipart:
`datasetName`, `run_in_background`, and the text as the `data` part, named
after its content so a repeat write is the server's no-op rather than a
`409`), `POST /api/v1/forget` (`dataset`, or `datasetId` + `dataId`),
`GET /api/v1/datasets` and `GET /api/v1/datasets/<id>/data[/<id>/raw]`.
Authenticated with `X-Api-Key` whenever a key is set, as the original sends
it.

## Not ported

- **Local-server mode.** The original can download `cognee==1.6.5` from PyPI
  into a private venv and start a server for you. A cell bundles no Python:
  point `base_url` at a cognee you started, or at a remote one.
- **Per-turn capture and the session bridge.** The original sends every turn
  to a session cache and runs `improve()` at session end to promote it into
  the graph; here the model calls `cognee_remember` (its description says
  when), and nothing is captured behind the user's back.
- **`cognee_code_search` and `cognee_recap`.** The deterministic code-graph
  lane needs repositories indexed by the original's CLI, and recaps read the
  harness's own session table; neither has a Nodecode seam here, so their
  subjects — indexed code, recorded sessions — are not claimed.
- **The `> Cognee:` credit line and the memory-hit counter.** Both are
  Hermes prompt decorations; a cell contributes tools, a command and a
  manual page, not prompt text.

Apache-2.0, topoteretes's.
