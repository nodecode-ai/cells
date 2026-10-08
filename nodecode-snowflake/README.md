# nodecode-snowflake

A Nodecode cell for [Snowflake Cortex](https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-rest-api)'s
REST API. With it, `/connect` offers Snowflake Cortex, `/models` lists the
models oh-my-pi bundles for it (Claude Opus, Sonnet and Haiku, GPT-5 and
GPT-4.1), and a turn on `snowflake/<model>` goes to your account the way
oh-my-pi sends it:

- the address is the account's own host,
  `https://<org>-<account>.snowflakecomputing.com`: Claude on the Messages
  wire at `/api/v2/cortex/v1/messages`, GPT on the chat wire at
  `/api/v2/cortex/v1/chat/completions`
- the bearer is a programmatic access token (PAT) or a Snowflake OAuth
  access token, sent as `Authorization: Bearer`, and nothing else
- a chat round names its cap `max_completion_tokens` (Cortex refuses
  `max_tokens`) and sends no `prompt_cache_key`
- an account is normalized as omp does: `orgname-accountname`, an account
  URL, or a Snowsight link (`app.snowflake.com/<org>/<account>`) all become
  the account's `https` origin; plain http, another host, a China-region
  account and a legacy Snowsight link are refused before anything is sent

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `snowflake`
provider and sign-in. See [NOTICE](NOTICE).

## Sign in, or a PAT

omp's login is `custom`: it asks for the account first, then runs Snowflake
OAuth for local applications. This cell ports it as a command, because the
flow is a browser sign-in, not a secret to paste:

```
/snowflake login myorg-myaccount    the address to open; the account may be its URL
/snowflake code ADDRESS             the address the browser ended on, when it runs elsewhere
/snowflake status                   which account, and how long the token has left
/snowflake logout                   forgets the sign-in
```

`login` without an account signs in to the section's `account`, else
`SNOWFLAKE_ACCOUNT`. The sign-in is the built-in `LOCAL_APPLICATION` client
with PKCE and the `refresh_token` scope, on the account's own
`/oauth/authorize` and `/oauth/token-request`. The browser returns to
`http://127.0.0.1:54551/`, or to another free loopback port when that one is
taken, as omp's callback falls back. The command answers at once, and a
notice says when you are signed in.

The token is kept in Nodecode's shared `auth.json` under
`oauth_tokens.snowflake`:

```json
{"provider": "snowflake", "access_token": "...", "refresh_token": "...",
 "expires_at": 1791234567, "account_url": "https://myorg-myaccount.snowflakecomputing.com"}
```

`expires_at` is in epoch seconds. A token that expires within a minute is
refreshed before a round sends it, and written back; when Snowflake issued
no refresh token (`refresh_token` is `""`) the token stands until it expires,
and then the round says to sign in again. Every other field of `auth.json`
is kept, and the file stays mode 0600.

A PAT is the other way in, as omp reads it: save it with `/connect` or set
`SNOWFLAKE_PAT`, and name the account in the section's `account` or
`SNOWFLAKE_ACCOUNT`. The order a round's credential is found in:

1. a PAT `/connect` saved (the core reads `api_keys` before any cell)
2. the sign-in
3. `SNOWFLAKE_PAT`

A saved key may also be omp's structured key, `{"token": "...",
"enterpriseUrl": "https://..."}`; its account is used. The account a round
goes to is the sign-in's, else the structured key's, else the section's,
else `SNOWFLAKE_ACCOUNT`.

`/connect` does not check a PAT: omp treats this roster as
credential-scoped and never fetches it, so the picker is answered from the
bundled rows.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-snowflake/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

```jsonc
{
  "snowflake": {
    // orgname-accountname or the account URL (else SNOWFLAKE_ACCOUNT)
    "account": "myorg-myaccount"
  }
}
```

## Prices

omp prices each model at its AI credits times $2, the on-demand global
rate; regional routing and contracts bill differently.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-snowflake
```

The sign-in's callback listens on a free loopback port the test dials. Every
Snowflake endpoint is a stubbed `dex:request` or `dex:post`.

## Not ported

- omp's check that a sign-in's account matches a model row carrying an
  address of its own: every row here is on the account's host.

MIT licensed.
