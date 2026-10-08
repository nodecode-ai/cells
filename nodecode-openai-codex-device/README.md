# nodecode-openai-codex-device

A Nodecode cell that signs in to a ChatGPT Plus or Pro subscription (the
Codex subscription) with a device code. Use it on a machine with no browser,
or one whose browser cannot reach the machine's own loopback, such as a
server over SSH. Once you are signed in, `/models` lists the Codex models
under `openai-codex/`, and a turn on one of them goes to the ChatGPT backend
the way the Codex CLI sends it.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`openai-codex-device` sign-in and the `openai-codex` provider it signs in
to. See [NOTICE](NOTICE).

## Sign in

```
/openai-codex-device login
```

The command answers with `https://auth.openai.com/codex/device` and a code.
Open the address on any device, phone included, sign in, and type the code.
This machine asks OpenAI every few seconds (OpenAI's interval plus three),
up to 120 times, and a notice says when you are signed in. Nothing listens
on this machine.

## Where the sign-in is kept

omp keeps a device sign-in as `openai-codex`, not under a name of its own
(`store-as "openai-codex"` in its rule). It is the same subscription however
you signed in, so this cell does the same. The token goes into Nodecode's
shared `auth.json` under `oauth_tokens.openai-codex`:

```json
{"provider": "openai-codex", "access_token": "...", "refresh_token": "...",
 "id_token": "...", "expires_at": 1791234567, "account_id": "...",
 "email": "...", "org_id": "...", "org_name": "plus", "installation_id": "..."}
```

`expires_at` is in epoch seconds. A token that expires within a minute is
refreshed before a round sends it, and the refreshed entry is written back.
Every other field of `auth.json` is kept, and the file stays mode 0600.
`/openai-codex-device status` says who is signed in, and
`/openai-codex-device logout` takes the entry out.

This cell also serves the `openai-codex` provider itself: the catalog row,
the Codex lane, the credential with its refresh, and the request shape. So it
works alone. The wire is the one `nodecode-openai-codex` carries. See that
cell's README for what a round carries and what is not ported.

## Beside nodecode-openai-codex

You can install both and sign in with whichever suits the machine. Both read and
refresh the one `oauth_tokens.openai-codex` entry by the same rule. Whichever
cell's hook runs first answers the credential, and the other is never asked.
Each registers a lane of its own name and shapes only the rounds on it.
While `nodecode-openai-codex` runs, the provider rides its lane,
`openai-codex`. Otherwise it rides this cell's lane, `openai-codex-device`. A
round is shaped once. If you set `base_url`, set it in both sections.

## Beside nodecode-codex-auth

This cell's lane has the family `:openai-codex`. codex-auth answers only
OpenAI-family lanes, so it never answers for `openai-codex`.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-openai-codex-device/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "openai-codex-device": {
    // the Codex backend: its root, its /codex, or its /codex/responses
    "base_url": "https://chatgpt.com/backend-api",
    // the client name every request carries
    "originator": "codex_cli_rs"
  }
}
```

## Not ported

Everything `nodecode-openai-codex` leaves out, for the same reasons: usage
and quota, the WebSocket transport, zstd bodies, the attestation header,
provider-native compaction, live model discovery, and stable effort updates.

## Gaps

- **Response headers.** As in `nodecode-openai-codex`, the turn's
  sticky-routing token and models etag are read by advising
  `NLE::NOTE-BODY-WIRE`. The seam it stands in for is a `:response-headers`
  keyword on `NLE::WALK-PROVIDER-STREAM`.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-openai-codex-device
```

Every OpenAI endpoint is a stubbed `dex:post`, and the wait between two polls
is stubbed out, so nothing reaches OpenAI and nothing sleeps.

MIT licensed.
