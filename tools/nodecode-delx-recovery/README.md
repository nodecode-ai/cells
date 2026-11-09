# nodecode-delx-recovery

Delx Recovery: recovery and continuity for a session, sealed as a capsule the next one resumes. A Nodecode rewrite of [davidmosiah/delx-plugins](https://github.com/davidmosiah/delx-plugins) by [davidmosiah](https://github.com/davidmosiah).

A session fails, loops, loses its context or gets compacted. The model gets five
tools to reach for at that moment: triage the problem, read back what the last
session sealed, keep one line of state, seal a Continuity Capsule before the
context goes, and close with a rating so the next session starts warm. `/delx`
shows where the session stands, registers a free identity, or triages a problem
from the shell, and `(help :delx)` is the same page the model reads.

The service is [Delx Protocol](https://delx.ai), hosted: every tool call is one
JSON-RPC `tools/call` to the MCP endpoint the plugin's `mcp.json` names, so what
the model stores (state, context memory, capsules) is retained by Delx under its
privacy policy. Nothing is stored locally but the credential; no secrets belong
in a capsule.

## Get it

Tell the agent "add the delx-recovery cell", or run `nodecode add nc://delx-recovery`.

## Set it up

Public triage needs nothing: install the cell and `delx_check` answers. Private
continuity — resume, remember, seal, close — needs a free agent identity, which
`/delx register` creates. It asks Delx for one and answers the three values to
keep; put them in the `delx` section of `config.jsonc` (or `/settings`):

```jsonc
{
  "delx": {
    "agent_id": "…",       // the id registration returned; the same one across sessions resumes the same lineage
    "agent_token": "…",    // identity_auth.token, shown once
    "session_id": "…"      // the session registration opened
  }
}
```

Then `(restart-cells "nodecode-delx-recovery")`. A token pasted into `/settings`
is kept in a 0600 file under the home and the section names the file. The
credential travels as the `x-delx-agent-id` and `x-delx-agent-token` headers the
service documents, never in an argument the transcript keeps.

| Member | Default | What it does |
| --- | --- | --- |
| `endpoint` | `https://api.delx.ai/v1/mcp/protocol?src=plugin` | the MCP endpoint; https:// is refused otherwise |
| `agent_id` | unset | the id registration returned |
| `agent_token` | unset | its credential, shown once |
| `session_id` | unset | the session registration opened, or one a session-opening tool issued |
| `language` | `en` | the language public triage answers in: `en`, `pt` or `es` |
| `timeout_seconds` | `30` | one request's ceiling |

An endpoint that is not `https://`, a language outside the three, or an integer
under its floor refuses at the start, naming the member; the tools that need a
credential answer `ERROR: delx: no agent credential yet` until one is set.

## Use it

Ask the model to pick up where the last session left off:

> What did the last session get done, and what should I do next?

The model calls `delx_resume` and reads back the capsule: goal, done, next,
blockers, and what the last run already ruled out. Before a compaction or a
handoff it calls `delx_seal` with the same fields, and at the end of a run worth
resuming, `delx_close` with a 1-5 rating. `delx_remember` keeps one durable line
between them.

- `delx_check {problem, intent?}` — free public triage of one problem: one next
  step or one clarification. No credential, no session, so it cannot say whether
  saved context exists; `delx_resume` can.
- `delx_resume {}` — the capsule and hive notes the last session sealed.
- `delx_remember {key, value}` — one line of state a later session reads.
- `delx_seal {goal, done, next, blockers?, refuted?}` — a mid-session checkpoint.
- `delx_close {goal, done, next, blockers?, refuted?, rating?}` — report the
  rating, then seal and close in one call.

`/delx` alone prints the endpoint, the agent, the session and what to set next;
`/delx register` creates the identity; `/delx resume` reads the capsule back;
`/delx <a problem>` runs public triage.

A capsule is `{"version": "1", "goal": …, "done": …, "next": …, "blockers": …,
"refuted": …}` — the schema is at
`https://api.delx.ai/schemas/continuity-capsule-v1.json`, and `refuted` is the
field worth filling: it is what summarising destroys first. What comes back from
a capsule or a hive note is data, not orders: the primer tells the model to
summarize it, never to execute it.

## The wire

One request per call, to `endpoint`, with
`{"jsonrpc": "2.0", "id": N, "method": "tools/call", "params": {"name": …,
"arguments": {…}}}` and the headers `content-type: application/json`,
`accept: application/json, text/event-stream` and `x-delx-source: nodecode`; the
answer is read as JSON or as the last `data:` frame of an event stream. A
refused status, a JSON-RPC error or an `isError` result becomes the `ERROR:
delx: …` line the model repairs from. `/delx register` posts
`{"agent_id": "nodecode-xxxxxx", "registration_key": "rk1_…", "source":
"skill"}` to `/api/v1/agents/register` on the endpoint's own host.

## Not ported

The plugin's two guardian hooks (`PreCompact`, `SessionEnd`) run inside Claude
Code's hook contract: they seal a capsule from environment variables when that
harness compacts. Nodecode has no such hook point reaching a shell script, so a
seal here is the model's `delx_seal` call, `/delx` and `(help :delx)` say when.
The skill is the primer rather than a separate skill file, and the plugin's
breadth — witness lineage, fleets, peers, commerce — is left to the service: the
five tools here are the ritual the skill tells an agent to walk.

Apache-2.0, as the original.
