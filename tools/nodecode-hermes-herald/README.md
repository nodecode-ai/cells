# nodecode-hermes-herald

Hermes Herald: dispatch tasks to a Hermes Agent profile's API server, poll and cancel the runs, chat with it, and list the models it can run. A Nodecode rewrite of [bennybuoy/hermes-herald](https://github.com/bennybuoy/hermes-herald) by [bennybuoy](https://github.com/bennybuoy).

A Hermes Agent profile can serve an OpenAI-compatible API server (`/v1/runs`, `/v1/runs/{id}`, `/v1/chat/completions`, `/v1/models`, `/v1/health`). The original plugin was a Hermes-side toolset that dispatched work to *other* profiles of the same installation, by name. This cell is the same conversation with one address instead of a list: the profile you point it at can be on this machine or behind a tunnel, and it needs no Hermes plugin on this side.

| Tool | What it does |
| --- | --- |
| `hermes_dispatch` | Starts a self-contained task on the profile and answers a run id at once; the run keeps going there. |
| `hermes_check` | Reads one run's status, or several at once: running, completed, failed, cancelled, waiting for approval. |
| `hermes_cancel` | Asks the profile to stop a run; cancellation is cooperative. |
| `hermes_models` | The model route aliases the profile advertises for dispatch, and what each resolves to. |
| `hermes_chat` | One synchronous turn of a conversation the profile keeps, for asks whose later turns depend on earlier ones. |
| `hermes_runs` | The runs dispatched from this machine, newest first, as this side recorded them. |

Ask "have the reviewer profile check this diff and tell me what breaks", then "is it done yet?", then "stop it" — or `/hermes`, `/hermes runs`, `/hermes models` from the prompt.

## Get it

Tell the agent "add the hermes-herald cell", or run `nodecode add nc://hermes-herald`.

## Set it up

The cell talks to a Hermes Agent profile whose API server is switched on. In that profile's own config:

```yaml
platforms:
  api_server:
    enabled: true
    extra:
      model_routes:
        reviewer-fast: {provider: ollama-cloud, model: glm-5.2}
```

Then, in `/settings` (or in the `hermes` section of `config.jsonc`):

```jsonc
"hermes": {
  "name": "reviewer",
  "url": "http://hermes.local:8651",
  "api_key_env": "HERMES_API_KEY",
  "model": "reviewer-fast",
  "timeout_seconds": 60
}
```

- `url` is the profile's API-server base address, scheme included. A remote one needs TLS or a trusted private network; the cell adds none.
- `api_key_env` names the environment variable holding the server's bearer token — the key Hermes prints for `api_server`, not a model-provider key. Unset, requests carry no token, which a loopback server may accept.
- `model` is the alias sent when a call names none; leave it out and the profile's own default answers.
- The aliases you can dispatch to are the profile's `model_routes`, not its whole provider catalog: `hermes_models` lists exactly those.

## Use it

`hermes_dispatch` is asynchronous: it answers a run id, and the work continues on the profile with its own session, model, skills and tools. The brief does not inherit this conversation, so write the goal, the paths, the constraints and how to check the result into `message`.

```
hermes_dispatch(message="Review the diff in ~/work/parser. Report only real defects, with file:line.", model="reviewer-fast")
→ {"run_id": "4f2a9c1e", "status": "submitted", ...}

hermes_check(run_ids=["4f2a9c1e"])
→ {"runs": [{"run_id": "4f2a9c1e", "status": "running", ...}]}
```

A profile's final summary is a self-report: read back the file, the commit or the URL it names before you report the work as done. `hermes_chat` blocks the turn until the profile answers, so keep it for short back-and-forth; `hermes_cancel` waits for the step the profile is on.

The runs this cell started from here are kept in `hermes-herald/runs.json` under the Nodecode home (mode 0600), newest hundred, with each brief's opening words; `hermes_runs` and `/hermes runs` read that record, and the profile stays the authority on a run's live status. The chat session handle is kept beside it in `hermes-herald/session.txt`.

## What the rewrite leaves out

The Hermes-only half of the original: `delegate_subagent` and `llm_call` run inside Hermes's own agent and provider routing, which a cell's tools do not have; `llm_direct` called a configured OpenAI-compatible endpoint directly; the approval relay and the SSE callback listener delivered results into a Hermes session as they arrived. Dispatch and chat here are the same HTTP wires, but the answer to a dispatch is the run id and a poll — no push — and the local SQLite audit ledger is the JSON record above. See [NOTICE](NOTICE).
