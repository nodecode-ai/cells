# nodecode-account-pool

Account Pool: several Claude and Codex accounts in one pool -- each one's
usage windows, which account to send the next request to, and a park switch
that keeps a spent one out of the rotation. A Nodecode rewrite of
[roqianjas/hermes-account-pool](https://github.com/roqianjas/hermes-account-pool)
by [roqianjas](https://github.com/roqianjas).

One GET per account, to the vendor that owns that account's token: Anthropic's
OAuth usage endpoint for a Claude subscription, ChatGPT's usage endpoint for a
Codex one. Nothing is shared between accounts and nothing goes anywhere else.
A wake -- one tiny request that starts an idle account's 5-hour window -- goes
to that same vendor. Park decisions are a plain text file under the home.

## Get it

Tell the agent "add the account-pool cell", or run `nodecode add nc://account-pool`.

## Set it up

List one entry per account in the cell's `accounts`, as
`label:provider:ENV_VAR`, with the provider `anthropic` or `openai-codex`.
`label:ENV_VAR` is a Claude account, which is the common case:

```jsonc
{
  "account-pool": {
    "accounts": [
      "claude-main:anthropic:CLAUDE_MAIN_TOKEN",
      "claude-backup:anthropic:CLAUDE_BACKUP_TOKEN",
      "codex-main:openai-codex:CODEX_MAIN_TOKEN"
    ]
  }
}
```

Each entry names the environment variable holding that account's access
token: the token of that account's own sign-in (a Claude Code sign-in's
OAuth access token, or a ChatGPT Plus/Pro sign-in's). The config file holds
no secret, only the names. An account whose variable is unset still appears
in the report, saying which variable to export; with none set the cell
refuses at start and names them all.

The rest of the section: `cache_seconds` (60) bounds how long one account's
answer is reused, `timeout_seconds` (12) one usage read, `wake_timeout_seconds`
(60) one wake, `wake_model` (`claude-haiku-4-5`) and `codex_wake_model`
(`gpt-5.4-mini`) are the models a wake sends to, and `state_file`
(`account-pool/parked.txt`) is where park decisions are kept. See
`config.example.jsonc`.

## Use it

Ask, and the model calls the tool:

| Tool | What it does |
| --- | --- |
| `account_pool` | `report`: every account's usage windows, what is left and when it resets, and whether it is in rotation; `best`: which account to use next; `park` and `unpark`: a label in or out of the rotation; `wake`: one tiny request so an idle account's 5-hour window starts now |

By hand: `/account-pool` prints the report, and `/account-pool best`,
`/account-pool park LABEL`, `/account-pool unpark LABEL`,
`/account-pool wake LABEL` and `/account-pool refresh` do the rest. From eval:

```lisp
(account-pool:report)                    ; the text /account-pool prints
(account-pool:report :account "claude-main" :refresh t)
(account-pool:best)                      ; most headroom first
(account-pool:windows "codex-main")      ; :windows, :details, :reason as a plist
(account-pool:park "claude-backup" :hours 6)
(account-pool:wake "claude-backup")
```

An account that answers a refusal, a limit already reached, or no window is
one line of the report, not a failure. A missing label reads
`ERROR: ACCOUNT-POOL-ERROR` and lists the ones the pool holds.

## What it does not do

The original also adds and verifies sign-ins through Hermes's own credential
pool, rotates a running session between accounts, keeps token counts per
account and session, schedules weekly parks, runs a five-minute balance timer
and an alerting channel, and draws a Hermes Desktop page. Here the accounts
are the section's list, the switch is `park`, and the report is text.

MIT, see [LICENSE](LICENSE).
