# nodecode-devin

A Nodecode cell for [Devin](https://devin.ai): Codeium's Cascade backend,
the models a Devin account is entitled to (SWE-1.6 and SWE-1.7, Claude, GPT,
Gemini, GLM and the rest of the account's roster). With it, `/devin login`
signs in with the browser, `/models` lists the account's roster, and a turn on
`devin/<model>` goes to `https://server.codeium.com` the way the Devin CLI
sends it.

Cascade speaks none of the organism's four wires: it is the Connect protocol
over HTTP/1.1 with protobuf messages. The cell registers a lane of its own,
`devin`, which makes the exchanges itself:

- `GetUserJwt` (unary) turns the session token into a user JWT, and names
  the account's own chat host when it has one
- `AssignModel` (unary) resolves a router model (`adaptive`) into a concrete
  uid and an assignment JWT
- `GetChatMessage` (server streaming) sends the conversation as one gzipped
  Connect frame and folds the answer's frames into text, thinking (with its
  signature, replayed next round to the model that made it), tool calls and
  usage; the end-of-stream frame's error fails the round
- `GetCliModelConfigs` (unary) lists the account's roster for `/models`

The protobuf messages (exactly the fields omp sets and reads), the Connect
envelope and gzip are written by hand in `proto.lisp`.

A Cascade roster names one uid per reasoning effort
(`claude-opus-5-low` ... `claude-opus-5-max`). As omp does, the cell folds
each such family into one model whose effort picks the uid: from the family
metadata the server ships, and from omp's reviewed Devin families
(`wire.json`).

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `devin`
provider. See [NOTICE](NOTICE).

## Sign-in

```
/devin login        answers the address to open; Nodecode listens at
                    http://127.0.0.1:59653/callback for Devin's answer
/devin code ADDRESS the address the browser landed on, when it runs elsewhere
/devin status
/devin logout
```

The sign-in is Devin's CLI grant (authorization code with PKCE) at
`https://app.devin.ai/auth/cli/continue`, its code exchanged at
`https://api.devin.ai/auth/cli/token` for a session token. auth.json keeps it
under `oauth_tokens.devin`:

```json
{"access_token": "...", "refresh_token": "...", "expires_at": 1767225600,
 "api_endpoint": "https://api.devin.ai", "enterprise_url": "https://app.devin.ai"}
```

`expires_at` is the token's own `exp` less five minutes, or a year from the
sign-in when it names none. omp has no refresh for Devin, and neither does
the cell: an expired sign-in is set aside, a round asks for `/devin login`
(a standing notice says so) and `DEVIN_API_KEY` answers in its place when it
is set.

`DEVIN_API_KEY`, or a key `/connect` saved, works without a browser sign-in.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-devin/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "devin": {
    // where Cascade is served
    "base_url": "https://server.codeium.com"
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-devin
```

## Regenerating the data

```sh
python3 -I tools/omp-models.py ~/.cache/nodecode-cells/omp-src devin nodecode-devin/models.json
python3 -I nodecode-devin/tools/wire-rows.py ~/.cache/nodecode-cells/omp-src nodecode-devin/wire.json
```

## Where this differs from omp

Each of these is a choice made here, said so it is not mistaken for omp's:

- **The request is gzipped without compression.** The image links no
  compressor (dexador's chipz only inflates), so the request frame is a gzip
  member of stored deflate blocks: valid gzip, as large as the protobuf it
  carries. Answers are inflated with chipz, compressed or not.
- **The kept request has no secrets.** A lane's fourth value is the request
  the organism keeps (its sha256 on the provider_request fact, its bytes in
  request_bodies). Cascade carries the session token and the user JWT inside
  the body's Metadata, so the cell answers the GetChatMessageRequest with
  both left empty, not the exact bytes sent.
- **Another model's thinking is demoted as plain text.** omp renders it in
  the target model's own thinking dialect (`renderDemotedThinking`); the
  dialect registry is not carried, so the text goes ahead of the answer as it
  is, followed by a newline (omp's anthropic-dialect form).
- **A trailer error keeps Connect's status.** omp raises every error an
  end-of-stream trailer carries as a validation error, never retried; here
  the trailer's Connect code maps to the HTTP status Connect pairs with it
  (`invalid_argument` 400, `unauthenticated` 401, `unavailable` 503, ...),
  so the organism's retry policy retries the 5xx and 429 ones. The large
  history `invalid_argument ... internal error` case omp flags as a context
  overflow says `context` in its detail, which is how the organism's
  eviction recognizes an overflow.
- **The Cascade id is derived from the session.** omp passes its session id
  as `cascade_id`; the cell passes a UUID derived from the Nodecode session
  id, so every round of a session shares it and the id has the shape the
  server issues.
- **A clean end without the end-of-stream frame ends the round**, as omp's
  reader does; an end inside a frame is a truncated stream.
- **Discovery is kept in memory.** omp caches the discovered roster; the
  cell asks again after a restart, on the first `/models` listing or the
  first round of a model the seeds do not name. Until then omp's reviewed
  families still route an effort to its uid.
- **The tool schema normalization for Devin's Gemini backend is compact**:
  references resolved, a type list made one type and `nullable`, constants
  and enums as strings, unsupported keywords dropped with their constraints
  said in the description; not every option of omp's `normalizeSchemaForGoogle`.

- **The X / X-thinking pair rule is not carried.** omp's global
  `deriveThinkingPairFamilies` folds any same-priced `X` and `X-thinking`
  pair into one model, its effort ladder read from omp's model policy
  (`resolveModelPolicy`), which this cell does not carry. Such a pair lists
  as two models here; the server's own families and omp's reviewed ones are
  folded.

## Gaps

What omp does that the cell cannot carry without a core change:

- **Which model wrote a message.** omp replays an assistant message's
  thinking, its signature and the id Cascade gave it only to the model that
  wrote it (`isNativeDevinMessage`, which reads the message's own api,
  provider and model). The organism's assistant message carries no author,
  and a field the lane added would ride every later round to whatever lane
  serves it (the chat lane replays a message's keys verbatim), so the cell
  keeps the author beside the message, in memory: a table keyed by the
  digest of the message's text, reasoning and tool-call ids, bounded at 4096
  messages and cleared at the limit and when the cell stops. After a restart
  (or past the bound) a message's author is unknown, and what is lost is
  exactly this: the round replays that message's thinking demoted to text
  instead of as thinking, drops its signature, and names it by the derived
  `bot-<uuid>` id instead of Cascade's own message id. It needs the stored
  assistant message to carry the provider and model that wrote it (a field
  the lanes do not send), or a per-message side record the store keeps for
  a lane.

- **Credits.** Cascade reports `credit_cost`, `committed_credit_cost` and
  `committed_acu_cost` per round, which omp keeps on its usage. The cell
  decodes them and drops them: `nle::provider-usage` has token slots only.
  Carrying them needs a slot there (or a usage plist a lane may extend) and a
  reader that shows it.
- **Tool results that failed.** omp sends `tool_result_is_error` on a failed
  tool's result. The organism's tool message (`nle::message` "tool") carries
  no failure mark, so the field is never set. It needs the mark on the
  message the turn loop builds (`RUN-TOOL-CALL`'s refusal path).
- **Selector aliases.** omp resolves Devin's short names (`opus`, `sonnet`,
  `swe`, `gpt-5.6-sol`, ...) to logical models when a selector names the
  provider. The cell honours them on the wire (`MODEL-SPEC`), but the
  organism's model selection has no per-provider alias seam, so `/models`
  and `(select-model ...)` know only the listed ids.
MIT licensed.
