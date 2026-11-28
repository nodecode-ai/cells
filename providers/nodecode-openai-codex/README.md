# nodecode-openai-codex

A Nodecode cell for a ChatGPT Plus or Pro subscription (the Codex
subscription). Sign in once from `/openai-codex`, and `/models` lists
the Codex models under `openai-codex/`. A turn on one of them goes to the
ChatGPT backend, `https://chatgpt.com/backend-api/codex/responses`, the way
the Codex CLI sends it:

- the bearer is your sign-in's access token, refreshed before it expires,
  and the ChatGPT workspace it draws on rides as `chatgpt-account-id`
- every request names its client (`originator`, `version`, `OpenAI-Beta`, a
  routing hint) and its place in the conversation (session, thread, window
  and turn, as headers and as `client_metadata`)
- no output cap and no sampling control is sent (the backend refuses them),
  no input item names an item id (the backend stores nothing), and encrypted
  reasoning is always asked for
- the stream may end on `response.done`, and a `response.failed` is reported
  as a failure with the backend's reason
- the sticky-routing token and models etag the backend sends in its response
  headers ride on the turn's next request

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `openai-codex`
provider and sign-in. See [NOTICE](NOTICE).

## Sign in

```
/openai-codex → Sign in
```

The command answers with an address on `auth.openai.com`. Open it in a
browser and sign in. OpenAI sends the browser back to
`http://localhost:1455/auth/callback`, which this cell listens on for five
minutes, and a notice says when you are signed in.

OpenAI accepts only that one redirect for this sign-in, so port 1455 must be
free. If it is busy, the command says so and nothing else happens.

If the browser runs on another machine, the redirect lands on a page that
does not load. Copy that page's address and send it:

```
/openai-codex → Paste where the browser landed  like http://localhost:1455/auth/callback?code=...&state=...
```

On a machine with no browser at all, use the `nodecode-openai-codex-device`
cell instead. It signs in to the same subscription with a code you type on any
device.

The token is kept in Nodecode's shared `auth.json` under
`oauth_tokens.openai-codex`:

```json
{"provider": "openai-codex", "access_token": "...", "refresh_token": "...",
 "id_token": "...", "expires_at": 1791234567, "account_id": "...",
 "email": "...", "org_id": "...", "org_name": "pro", "installation_id": "..."}
```

`expires_at` is in epoch seconds. A token that expires within a minute is
refreshed before a round sends it, and the refreshed entry is written back.
Every other field of `auth.json` is kept, and the file stays mode 0600.
`/openai-codex` says who is signed in and how long the token has left,
and **Sign out** in `/openai-codex` takes the entry out.

When nothing is saved, a token in `OPENAI_CODEX_OAUTH_TOKEN` is sent instead.
That is the variable omp reads, and it is never refreshed.

## Beside nodecode-codex-auth

`nodecode-codex-auth` answers the credential for OpenAI-family lanes from
whatever `oauth_tokens` holds, without refreshing. This cell serves its own
provider, `openai-codex`, on a lane of its own whose family is
`:openai-codex`, not `:openai`. So codex-auth never answers for
`openai-codex`, and this cell never answers for any other provider. The two
can be installed together. They serve different providers out of one
`auth.json` and do not touch each other's entries.

## Beside nodecode-openai-codex-device

Both cells keep the sign-in under `oauth_tokens.openai-codex`, as omp does,
and both answer and refresh it the same way. Each registers a lane of its own
name (`openai-codex`, `openai-codex-device`) and shapes only the rounds on its
lane. While this cell runs, the provider rides the `openai-codex` lane, so a
round is shaped once. If you set `base_url` in one section, set it in the
other too.

## Models

The models offered are the ones the Codex backend lists for the signed-in
account: `GET <codex base>/models?client_version=V`, with the account's own
bearer. `models.json` is the fallback before the first answer and when the
backend cannot be reached; it is not the list.

- **What is asked.** The backend withholds a model from a client older than
  the model's `minimal_client_version`, without an error. The listing
  therefore asks with a ceiling version (`99.0.0`, or `client_version` when
  set), so a new model appears without a source change. A round names the
  version the cell was written against (`0.159.0`), or the chosen model's
  `minimal_client_version` when that is newer.
- **What is offered.** Rows with `visibility: list`. Hidden rows are not
  offered. Context window, reasoning efforts and input kinds come from the
  row; prices come from `models.json` where it knows the model. The image
  model is not in the backend's list and stays bundled.
- **Whose list.** One snapshot per provider id and account. A slot signed in
  to another account drops the earlier account's snapshot and the host's
  listing cache for that id; an answer that arrives after the switch is
  discarded. Only a digest of the account id is kept on disk
  (`openai-codex-catalogue.json` beside the host's provider-models cache,
  mode 600, no token).
- **When it is asked.** By the host's own listing refresh (`/models`, 600 s;
  `/models refresh` asks now). A round's `x-models-etag` that differs from
  the one the previous round carried drops the host's listing cache, so the
  host's next refresh asks. A round on a model that neither the snapshot
  nor `models.json` holds asks first, at most once a minute.
- **Where the bearer goes.** The cell lists only at
  `https://chatgpt.com/backend-api/codex`, never follows redirects, and
  refuses a different base before resolving a credential. A custom
  `base_url` still applies to rounds; it does not receive listing credentials.
- **When it fails.** The listing answers no rows and the reason; the host
  keeps the rows it had and shows the reason. `/openai-codex status` says
  per provider whether the catalogue is live or bundled, when it was asked
  and the last failure.

Not observed: a chat round on a model whose `minimal_client_version` is above
`0.159.0`. The version sent for it follows the backend's own row.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-openai-codex/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "openai-codex": {
    // the Codex backend: its root, its /codex, or its /codex/responses
    "base_url": "https://chatgpt.com/backend-api",
    // the client name the sign-in and every request carry
    "originator": "codex_cli_rs",
    // the client_version the model listing is asked with; empty asks with a ceiling
    "client_version": ""
  }
}
```

omp sends `originator: omp`, its own name. This cell sends `codex_cli_rs` by
default, the Codex CLI's name, which `nodecode-codex-auth` already sends.

## Prices

omp prices each Codex model at its OpenAI API list price, so the Dashboard
shows what a round would have cost on the API. A subscription round is not
billed per token.

## Not ported

- Usage and quota reporting (`usage/openai-codex*.ts`).
- The WebSocket transport. omp prefers it for some models and falls back to
  SSE. This cell always uses SSE, which the backend serves for every model.
- zstd compression of the request body. omp retries without it when the
  backend refuses it, so a plain body is accepted.
- The `x-oai-attestation` header. omp sends it only when a host app installs
  an attestation provider (DeviceCheck), and Nodecode has none.
- Provider-native compaction (`openai-codex-compaction.ts`, the remote
  compaction route). Nodecode shrinks its own context.
- The npm lookup omp uses to learn the newest Codex release. The listing
  asks with a ceiling `client_version` instead (see Models above), so no
  second network source decides what is offered.
- Effort carried as `configuration_update` items (GPT-6 Astra's stable
  effort). Each request sends its own effort.

## Gaps

- **Response headers.** The core gives no hook a round's response headers.
  This cell reads the sticky-routing token and models etag by advising
  `NLE::NOTE-BODY-WIRE (endpoint request-json session response-headers)`,
  the one function they reach today. That is an internal of the body-delta
  relay. The seam it stands in for is a `:response-headers` keyword on
  `NLE::WALK-PROVIDER-STREAM` that takes a function of the headers, called
  once per round before the stream is read.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-openai-codex
```

The sign-in's callback listens on a free loopback port the test dials. Every
OpenAI endpoint is a stubbed `dex:post`, so nothing reaches OpenAI.

MIT licensed.
