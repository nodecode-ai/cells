# nodecode-google-antigravity

A Nodecode cell for [Antigravity](https://antigravity.google), Google's agent
IDE, whose backend serves Gemini 3, Claude and GPT-OSS to a Google account on
its free tier. With it, `/models` lists the models under
`google-antigravity/`, `/google-antigravity login` signs in, and a turn on one
of them goes to `https://daily-cloudcode-pa.googleapis.com` the way omp sends
it, as the real Antigravity client does.

No lane of the organism speaks this wire, so the cell registers its own,
`google-antigravity`. Its stream is the organism's own Gemini fold with what
differs put around it:

- the request is the Gemini body inside Antigravity's envelope:
  `{"project", "requestId": "agent/<agent>/<ms>/<trajectory>/<step>",
  "request": {contents, systemInstruction (as the user's), tools,
  toolConfig, labels, generationConfig, sessionId}, "model", "userAgent":
  "antigravity", "requestType": "agent"}`; a session keeps one agent, one
  trajectory and one signed-decimal session id, its step counting up, and
  each request names the last answer's id
- requests carry the Antigravity client's user agent, its version read from
  Antigravity's update manifest (the backend gates newer models on it), and a
  thinking Claude model's interleaved-thinking beta
- the requested model is the effort's own (`gemini-3.1-pro` at high is
  `gemini-pro-agent`, at off `gemini-3.1-pro-low`), with the real client's
  fixed output cap and model label for it; a model that would think by
  default is told explicitly when thinking is off
- tools ride the legacy `parameters` field for every model; the tool mode is
  VALIDATED, and always for Claude; a forced call to a Gemini model is
  restated in the transcript (`forced-tool.md`), since those routes drop the
  tool config
- Claude's calls carry ids unique within the request, each result answering
  its own; Claude's unsigned thinking is dropped, not replayed
- a round that fails at the daily host before anything streamed (no answer,
  408, 429, 5xx) is tried at the sandbox host
  (`daily-cloudcode-pa.sandbox.googleapis.com`), and the host that answered
  is the session's first next time
- each event is `{"response": <Gemini chunk>}`; an in-band `{"error"}` and a
  blocked prompt are said as errors, a Flash model's leaked planning text is
  held back and dropped, and a stream that ends without a finish reason is a
  truncated one

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`google-antigravity` provider. See [NOTICE](NOTICE).

## Sign-in

```
/google-antigravity login                open Google's consent page
/google-antigravity code ADDRESS         the address the browser landed on
/google-antigravity status
/google-antigravity logout
```

`login` answers at once with Google's consent page (Antigravity's own OAuth
client), and listens at `http://127.0.0.1:51121/oauth-callback` (another free
port when that one is taken) for the browser to come back. When the browser
runs on another machine than Nodecode, paste the address it lands on with
`/google-antigravity code`. Nodecode exchanges the code, finds the account's
project (provisioning the free tier first when the account has none, and
saying Google's own reason when the account cannot have it) and says once how
it went. A failure stands as a notice until a sign-in succeeds.

The sign-in is kept in `~/.nodecode/auth.json` under
`oauth_tokens.google-antigravity`:

```json
{"access_token": "ya29...", "refresh_token": "1//...", "expires_at": 1791234567,
 "project_id": "...", "email": "me@example.com"}
```

`expires_at` is Google's expiry less five minutes, as omp keeps it. A round
whose token expires within a minute refreshes it first and writes it back; a
refresh Google refuses stands as a notice naming `/google-antigravity login`.
No other provider's key (`GOOGLE_API_KEY` among them) is ever sent to
Antigravity.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-google-antigravity/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "google-antigravity": {
    // where Antigravity's Cloud Code Assist is served
    "base_url": "https://daily-cloudcode-pa.googleapis.com",
    // auto (the daily host, then the sandbox), production or sandbox
    "endpoint_mode": "auto"
  }
}
```

`PI_AI_ANTIGRAVITY_VERSION` pins the client version instead of the update
manifest's; `PI_AI_ANTIGRAVITY_OS`, `_ARCH` and `_CL` override the rest of the
user agent, as in omp.

## Files

- `models.json`: omp's bundled rows as the catalog reads them
  (`tools/omp-models.py`); the two image models are listed as no chat models
- `wire.json`: the same rows as the wire reads them: the model a request
  names per effort, its lineage, how it thinks, omp's compat flags. Written by
  `python3 -I wire-rows.py OMP_CHECKOUT google-antigravity wire.json`
- `forced-tool.md`: omp's forced-tool directive, verbatim

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-google-antigravity
```

## Gaps

The wire is carried on the core's Gemini fold, with no core change. What is
not carried, and the seam it would need:

- Call ids. CALL-GOOGLE-STREAMING's fold mints its own `tool-<n>` per round
  and drops the wire's `functionCall.id` (Claude's `toolu_...`); the cell
  sends ids unique within each request instead, which Claude accepts. The
  fold would need to keep a call's wire id when the part carries one.
- Signatures. The assembled message (DEFINE-PROVIDER-LANE's MESSAGE) keeps
  one `reasoning_content` and one `content`, and a lane may add no field of
  its own to it (another lane sends a message's fields as they are). So the
  cell keeps a round's thought signature, its answer signature and the model
  that made them in a table of its own, keyed by the session and the
  message's reasoning, answer and calls; each call's signature rides the
  core's own `thought_signatures`. Three things follow: several thought
  blocks in one round replay as one block under the last signature; the
  table lives in this process, so after a restart an earlier round's thought
  replays unsigned (as text, or dropped for Claude) and a Gemini 3 first call
  carries the bypass sentinel; and a message the core rewrites (a
  continuation merged after a cut stream) is no longer found. The seam would
  be per-block reasoning on the message that names its producing model, as
  the Responses lane's `reasoning_items` does for its own wire.
- The first-event watchdog, and the host fallback on a silent first event.
  omp gives a Flash model 60 s and every other 300 s to send its first event
  and fails over to the other host when none comes; WALK-PROVIDER-STREAM has
  one idle deadline (the config's request timeout) and takes no first-event
  deadline. The seam would be a `:first-event-seconds` key on
  WALK-PROVIDER-STREAM signalling a request-scope error, which the cell's
  host fallback already takes.
- omp replays an empty STOP twice itself and names a thought-only answer; the
  core's own PROVIDER-EMPTY-RESPONSE retry stands in for both.
- Model discovery (omp's `fetchAvailableModels`, which is the account's
  served roster, with its variant collapsing): not ported. `/models` lists
  the bundled roster and asks no one, so a bundled model the account is not
  served answers 404; /connect's key check is told a key is not checked,
  since the credential is a sign-in.
- The session identity lives in this process: a restart starts each
  session's agent, trajectory and step again.
- The schema normalizer is a compact port of omp's (2,400 lines of
  `utils/schema/normalize.ts`): unsupported keywords, references, type
  lists, combiners collapsed for the legacy field, constants and enums,
  required names; not every combiner shape.
- Image generation (`gemini-3-pro-image`, `gemini-3.1-flash-image`) and
  Antigravity's web search: not ported.

MIT licensed.
