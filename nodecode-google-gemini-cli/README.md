# nodecode-google-gemini-cli

A Nodecode cell for Google Cloud Code Assist, the backend the
[Gemini CLI](https://github.com/google-gemini/gemini-cli) signs in to: Gemini
models on a Google account's free tier, or on a Google Cloud project. With it,
`/models` lists the models under `google-gemini-cli/`, `/google-gemini-cli
login` signs in, and a turn on one of them goes to
`https://cloudcode-pa.googleapis.com` the way omp sends it.

No lane of the organism speaks this wire, so the cell registers its own,
`google-gemini-cli`. Its stream is the organism's own Gemini fold with what
differs put around it:

- the request is the Gemini body wrapped with the account's project and the
  model it names: `{"project", "model", "request": {contents,
  systemInstruction, tools, toolConfig, generationConfig}}`, POSTed to
  `/v1internal:streamGenerateContent?alt=sse` with the Google token as bearer
  and the Gemini CLI's user agent and client metadata
- the history is converted the way omp converts its own: user text and
  images, the model's turns with the thought signatures it made (only to the
  model that made them; the cell keeps them, never the message), Gemini 3's bypass sentinel on an unsigned first
  call, every result of a step in one user turn, reasoning another model
  wrote as a fenced text
- tools ride `parametersJsonSchema`, with what Google's schema has no field
  for left out and its constraint said in the description
- effort follows omp: Gemini 3 as a thinking level, Gemini 2.5 as a token
  budget on its `-thinking` model, a model that cannot think off running at
  its own lowest level
- each event is `{"response": <Gemini chunk>}`; an in-band `{"error"}` and a
  blocked prompt are said as errors, the planning text a Flash model leaks
  into its answer (`{"thought": ...}`) is held back and dropped, and a stream
  that ends without a finish reason is a truncated one

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`google-gemini-cli` provider. See [NOTICE](NOTICE).

## Sign-in

```
/google-gemini-cli login                 open Google's consent page
/google-gemini-cli code ADDRESS          the address the browser landed on
/google-gemini-cli status
/google-gemini-cli logout
```

`login` answers at once with Google's consent page, and listens at
`http://127.0.0.1:8085/oauth2callback` (another free port when that one is
taken) for the browser to come back. When the browser runs on another machine
than Nodecode, paste the address it lands on with `/google-gemini-cli code`.
Nodecode exchanges the code, finds the account's Cloud Code Assist project
(the one it has, else the free tier provisioned; a paid tier names its project
with `GOOGLE_CLOUD_PROJECT` or `GOOGLE_CLOUD_PROJECT_ID`) and says once how it
went. A failure stands as a notice until a sign-in succeeds.

The sign-in is kept in `~/.nodecode/auth.json` under
`oauth_tokens.google-gemini-cli`:

```json
{"access_token": "ya29...", "refresh_token": "1//...", "expires_at": 1791234567,
 "project_id": "...", "email": "me@example.com"}
```

`expires_at` is Google's expiry less five minutes, as omp keeps it. A round
whose token expires within a minute refreshes it first and writes it back; a
refresh Google refuses stands as a notice naming `/google-gemini-cli login`.
No other provider's key (`GOOGLE_API_KEY` among them) is ever sent to Cloud
Code Assist.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-google-gemini-cli/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "google-gemini-cli": {
    // where Cloud Code Assist is served
    "base_url": "https://cloudcode-pa.googleapis.com"
  }
}
```

## Files

- `models.json`: omp's bundled rows as the catalog reads them
  (`tools/omp-models.py`)
- `wire.json`: the same rows as the wire reads them: the model a request
  names, its lineage, how it thinks, omp's compat flags. Written by
  `python3 -I wire-rows.py OMP_CHECKOUT google-gemini-cli wire.json`

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-google-gemini-cli
```

## Gaps

The wire is carried on the core's Gemini fold, with no core change. What is
not carried, and the seam it would need:

- Call ids. CALL-GOOGLE-STREAMING's fold mints its own `tool-<n>` per round
  and drops the wire's `functionCall.id`; the cell sends ids unique within
  each request instead (Gemini takes none). The fold would need to keep a
  call's wire id when the part carries one.
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
- The first-event watchdog. omp gives a Flash model 60 s and every other 300 s
  to send its first event; WALK-PROVIDER-STREAM has one idle deadline (the
  config's request timeout) and takes no first-event deadline. The seam
  would be a `:first-event-seconds` key on WALK-PROVIDER-STREAM.
- omp replays an empty STOP twice itself and names a thought-only answer; the
  core's own PROVIDER-EMPTY-RESPONSE retry stands in for both.
- Model discovery (omp asks `retrieveUserQuota` for the account's models):
  not ported. `/models` lists the bundled roster and asks no one; /connect's
  key check is told a key is not checked, since the credential is a sign-in.
- The schema normalizer is a compact port of omp's (2,400 lines of
  `utils/schema/normalize.ts`): unsupported keywords, references, type
  lists, constants and enums, required names; not every combiner shape.
- omp's markup healing of `<thinking>` written inside an answer: the core
  routes a leading thinking tag only.

MIT licensed.
