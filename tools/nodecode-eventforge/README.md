# nodecode-eventforge

EventForge: verified engineering events, pending action approvals, connector drafts and workspace memory, as ten tools on one self-hosted control plane. A Nodecode rewrite of [sinequix/eventforge](https://github.com/sinequix/eventforge) by [sinequix](https://github.com/sinequix).

EventForge is a small self-hosted service that watches what happens to your code — a check run, a pull request, a deploy, an issue — and matches it against workflows. What a workflow wants to write becomes an *action proposal* that a person approves; what it learned becomes memory; connectors to the services it does not speak yet are drafted as isolated, reviewable MCP servers. This cell is a client of one such control plane over its own HTTP routes: it decides nothing, approves nothing by itself, and holds no copy of your data.

| Tool | What it does |
| --- | --- |
| `ef_list_events` | The events the plane holds: provider, topic, repository, signature status, processing state. A payload is trusted only when its signature was verified. |
| `ef_list_pending_actions` | The proposals waiting for a person: id, type, title, proposed diff, risk, capabilities, expiry. |
| `ef_approve_action` | Records approve or reject on one proposal. It records the decision; the plane performs the write. |
| `ef_list_workflows` | The configured workflows and the execution policy each runs under. |
| `ef_spawn_subagent` | Starts an analysis-only run for one workflow and one event. Its writes come back as proposals. |
| `ef_emit_event` | Adds a fact you hold as an untrusted event, which can trigger workflows and approve nothing. |
| `ef_listen_for_webhook` | Ensures the local relay is up and answers the provider's webhook endpoint. |
| `ef_forge_mcp` | Drafts a connector from a description: isolated, reviewable, not installed. |
| `ef_approve_forge` | Approves or rejects a draft, with exactly the capabilities it was reviewed with. |
| `ef_query_memory` | Searches one workspace and project's memory: earlier decisions, runbooks and lessons. |

Ask "what is waiting for a person in EventForge?", "approve the comment on the parser pull request", "why did the release workflow not run for last night's check?" — or type `/eventforge`, `/eventforge events`, `/eventforge replay <event-id>` at the prompt.

## Get it

Tell the agent "add the eventforge cell", or run `nodecode add nc://eventforge`.

## Set it up

Run the control plane on the machine that holds the repositories and the tokens — its own README says how — and then, in `/settings` (or in the `eventforge` section of `config.jsonc`):

```jsonc
"eventforge": {
  "api_url": "http://127.0.0.1:4310",
  "api_token_env": "EVENTFORGE_API_TOKEN",
  "workspace_id": "demo-workspace",
  "project_id": "eventforge-demo-service",
  "request_timeout_seconds": 10
}
```

- `api_url` is the plane's base address. Plain `http` is accepted only for `127.0.0.1`, `localhost` and `::1`; a plane anywhere else must be `https`, so the bearer token never crosses a wire in the clear. Credentials inside the URL are refused.
- `api_token_env` names the environment variable holding the plane's bearer token, for a protected plane. Leave it out for a loopback plane that asks for none; name it and leave the variable unset and the cell refuses at start, saying which variable it wanted.
- `workspace_id` and `project_id` are the scope a call that names neither reads and writes.
- `request_timeout_seconds` bounds one call.

`/eventforge status` says whether the plane answers and which build it is running; that is the one check worth doing first.

## Use it

The events are the interesting part, and the rule the original taught is kept: **only a verified signature makes a payload trusted**. `ef_list_events` withholds the payload of an unverified or demo event, and when you ask for it (`include_unverified_payload`) it comes back wrapped as `{"untrusted": true, "note": ..., "data": ...}` — evidence, never instructions. A tool that acts should quote the proposal it is deciding, because `ef_approve_action` is the same decision a person makes in the plane's console:

```
ef_list_pending_actions(event_id="3f9a1c7e-2b8d-4f60-a5e1-9c0b7d2e4a6f")
→ {"pending": [{"actionId": "9c1b7d2e-...", "title": "comment on parser#42", "risk": "low", ...}], "count": 1}

ef_approve_action(action_id="9c1b7d2e-...", approved=true)
→ {"status": "approved", ...}
```

Every EventForge tool call leaves one line in the plane's own audit log (`kind: nodecode_tool`, `<tool> ok in 12ms`), so the work is visible in EventForge beside the work a person did there; the call's arguments and its answer stay on this side. `/eventforge replay <id>` is the read-only view of one stored event: its signature status, its payload when verified, the runs it started, the proposals it produced and the audit lines that name it. It re-runs nothing.

## What the rewrite leaves out

The Hermes-only half: the pre-call hook that pushed newly verified events into the next request's context (`ef_list_events` answers the same facts), the approval gate that asked Hermes to prompt before the four irreversible tools (each tool's description says what it decides), the session-start health probe (that is `/eventforge status`), the auto-start that spawned the repository's prebuilt `eventforge-standalone.cjs` with node, and the triage skill, whose route walk is now the manual. See [NOTICE](NOTICE).
