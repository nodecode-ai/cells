# nodecode-cursor

A Nodecode cell for [Cursor](https://cursor.com): Claude, GPT, Gemini, Grok,
Kimi and Cursor's own Composer models through a Cursor account. With it,
`/models` lists the account's models, `/cursor login` signs in, and a turn on
`cursor/<model>` runs on Cursor's agent service the way the Cursor agent CLI
drives it.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `cursor`
provider. See [NOTICE](NOTICE).

## How a round works

Cursor does not take a chat request. It runs the agent loop itself, over its
own protocol: Connect framing over HTTP, protobuf messages (`agent.v1` in
omp's `agent.proto`). The cell registers a lane of its own, `cursor`, which
speaks it:

- a run is `POST /agent.v1.AgentService/RunSSE` (the server's messages,
  Connect-framed) plus one `POST /aiserver.v1.BidiService/BidiAppend` per
  client message: the run request, the answers to the server's asks, and a
  heartbeat every five seconds. This is omp's HTTP/1.1 transport; the appends
  ride a thread of their own while the round reads the stream
- the request carries the whole conversation: the system prompt and the
  history as JSON blobs (what the model reads) and as turn structures, each
  named by its SHA-256 and served from this process's blob store when the
  service asks for it over the key-value channel. The system prompt also
  rides as an always-applied rule, and Nodecode's tools as MCP tools, both
  given when the service asks for the request context
- the model id is routed the way omp routes it: the effort picks the wire
  sibling (`claude-opus-5-5` at `high` is `claude-opus-5-5-high`), an OpenAI
  effort sibling goes as its base id with a `reasoning` parameter, max mode
  follows each sibling's own marker, and an unknown pair is retried once with
  the discovery id as it is
- text, thinking and token counts stream into the round's message

The tool handoff is omp's external-executor contract
(`cursor-external-tool-handoff.md`): when the service asks for one of
Nodecode's tools (an MCP call), the cell answers "handed off to the external
client ... end the turn", files the call on the round's message, and the
service ends its turn. Nodecode's own turn loop runs the tool; the next round
carries the result in the history, and the service continues from there.

Every native tool the service asks for over the exec channel (shell, read,
write, grep, ls, delete, diagnostics, the `pi_*` family, subagents, ...) is
refused in its own typed result, as omp refuses it when no local handler is
installed, so the model turns to the tools Nodecode advertises. Hosted web
search and fetch are approved; questions, mode switches and plans are refused.

## Sign-in

```
/cursor login     prints the page to open; Nodecode polls until Cursor confirms
/cursor status
/cursor logout
```

The sign-in is Cursor's own browser poll: a PKCE verifier and a UUID, the
operator signs in at `cursor.com/loginDeepControl`, and the cell polls
`api2.cursor.sh/auth/poll` until Cursor hands over an access token and a
refresh token. auth.json keeps them under `oauth_tokens.cursor`
(`access_token`, `refresh_token`, `expires_at` in epoch seconds: the token's
own exp less five minutes, and `email` when the account's profile names it).
A round refreshes the token when it expires within a minute; a status
question never does. When Cursor ends the session (`shouldLogout`), the cell
says so in a standing notice until `/cursor login` succeeds.

Without a sign-in, `CURSOR_ACCESS_TOKEN` or `CURSOR_API_KEY` is sent as the
bearer. A token `/connect` saved answers before either; `/connect` checks it
by asking the account's roster (`GetUsableModels`) with it, and a refusal
reads as one (`HTTP 401 unauthenticated`).

## Install

Copy this folder into `~/.nodecode/cells/nodecode-cursor/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "cursor": {
    // where Cursor's agent service is served
    "base_url": "https://api2.cursor.sh"
  }
}
```

## Files

- `models.json`: omp's bundled rows as the catalog reads them
  (`tools/omp-models.py`)
- `wire.json`: what the wire reads of the same rows: the request id, the
  effort routing, the max-mode markers, the schema projection flag, the
  identity (`tools/wire-rows.py` in this folder writes it from an omp
  checkout; the cell never runs it)
- `external-tool-handoff.md`: omp's handoff text, verbatim

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-cursor
```

The tests write the expected protobuf bytes out by hand from `agent.proto`'s
field numbers, and run whole rounds against a fake service that answers the
cell's appends; nothing is dialled.

## Not carried from omp

Parts of omp's provider this cell does not do, and why:

- **HTTP/2.** omp runs `/agent.v1.AgentService/Run` as one HTTP/2 stream and
  falls back to RunSSE/BidiAppend. The image has no HTTP/2 client (dexador
  speaks HTTP/1.1), so the cell always uses the RunSSE/BidiAppend path, which
  omp also runs when told to (`transport: "http1"`).
- **Rich model discovery.** `/models` asks `GetUsableModels` (the account's
  runnable slugs, max-mode markers, windows from the bundled rows and 1M
  labels). omp also joins `AvailableModels` (parameter axes, per-variant
  routes, cost multipliers) and `GetDefaultModelForCli`; that join leans on
  omp's taxonomy compiler (variant collapse across the whole roster), which is
  not carried. So a request never names a discovered route: it uses the
  normalized pair, then the discovery id as it is.
- **The checkpoint resume.** omp retries a broken run itself, up to five
  times, from the newest checkpoint (`x-original-request-id`). Here a broken
  run is a provider error and Nodecode's own retry policy re-runs the round,
  rebuilt from the history. That is replay-safe because this lane never runs
  a tool during a run: every MCP call is only handed off.
- **Error classification.** A Connect error with Cursor's `ErrorDetails` maps
  to omp's statuses code by code. One without maps by the Connect protocol's
  own code-to-HTTP table (`resource_exhausted` 429, `unavailable` 503, ...);
  omp classifies those by message text.
- **Schema handling.** Tool schemas go as Nodecode holds them (omp upgrades
  each to JSON Schema 2020-12 first). The projection for the models that need
  it (Claude Fable) is a compact form of omp's: local `$ref`s inlined,
  `anyOf`/`oneOf`/`allOf` folded into object properties or dropped, never
  narrowing what is accepted.
- **Smaller things.** omp redacts credentials found in the system prompt;
  logs and the request-debug capture are omp's own tooling.

## Gaps

What the cell cannot carry without a core change:

- **Running a tool during a round.** omp answers Cursor's native exec asks by
  running its own tools in place (`execHandlers`: shell, read, write, grep,
  ls, delete, diagnostics, `pi_*`) while the service waits. A lane has no seam
  to dispatch one of the turn's tools and get its result while its stream is
  open: something like `(nle::run-tool-now name arguments) => result text`,
  recording the call and its result as the turn loop would. Without it every
  native ask is refused, and only MCP calls (Nodecode's advertised tools)
  reach the turn loop, one round each.
- **Calls the service resolves itself.** Cursor runs some calls on its side
  (todos, `connect_scm`, hosted web fetch, the edit tool) and omp shows each
  as a call already paired with its result; omp also files every refused
  native ask as a call paired with its refusal. The round's message has only
  `tool_calls`, which the turn loop runs, so these are not recorded. It would
  need a resolved-call entry the turn loop keeps with its result and never
  dispatches (provider-executed calls, which Nodecode leaves out by design).
- **Context occupancy.** Each checkpoint reports the conversation's used
  tokens (`token_details.used_tokens`, omp's `usage.contextTokens`);
  `nle::provider-usage` has no slot for it, so only input, output, cache and
  reasoning counts are reported.
- **Which model wrote a message.** Only a Kimi K3 round's thinking replays,
  and only to the same model, and K3 refuses history another model wrote; omp
  knows each message's provider and model. The organism's message carries
  neither, and a field the lane added would ride every other lane's replay
  of the history (the chat lane sends assistant messages verbatim), so the
  lane adds none. The cell keeps the writer of each round it ran in a table
  of its own, keyed by the round's digest (its text, its thinking, its call
  ids), in this process only. What is lost: after a restart, or for a round
  whose text a continuation changed, the writer is unknown, so K3 replays no
  thinking for that round; and a round another provider wrote is never known
  as foreign, so K3's refusal of foreign history covers only rounds another
  Cursor model wrote in this process. A seam would be the provider and
  model the turn loop already records per round (the provider-request fact),
  readable for each assistant message of the history.

MIT licensed.
