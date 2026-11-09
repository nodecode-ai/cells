# nodecode-protonpass

Proton Pass: read secrets out of your own vault with the `pass-cli` you already have, as tools the model calls. A Nodecode rewrite of [lucas-nicolas/hermes-protonpass](https://github.com/lucas-nicolas/hermes-protonpass) by [lucas-nicolas](https://github.com/lucas-nicolas).

## Get it

Tell the agent "add the protonpass cell", or run `nodecode add nc://protonpass`.

## Set it up

The cell runs [`pass-cli`](https://protonpass.github.io/pass-cli/), which is
yours: nothing here vendors it, downloads it or talks to Proton by itself. A
Proton Pass plan with CLI access is needed.

```sh
pass-cli login          # a terminal, once: the session the cell can see
```

A machine with no terminal to sign in at — a server — wants a token instead:

```sh
pass-cli pat create --name nodecode --expiration 1m
pass-cli pat access grant --personal-access-token-name nodecode --vault-name Keys --role viewer
```

Put it in the environment variable the section names, and the cell signs
`pass-cli` in with it, into a session directory of its own under the Nodecode
home, so a login of your own is never overwritten. An **AI-agent token**
(`pass-cli agent create`) also writes every read, with the reason the cell
gives, to an encrypted audit log — `pass-cli agent monitor nodecode` shows it.

Nothing is required in `~/.nodecode/config.jsonc`: an absent section is
enabled. What it may carry:

| Member | Default | What it does |
| --- | --- | --- |
| `command` | `["pass-cli"]` | the argv prefix that runs the CLI, or an absolute path when PATH does not have it |
| `token_env` | `"PROTON_PASS_PERSONAL_ACCESS_TOKEN"` | the variable holding a personal access or AI-agent token; unset means "use the login this process sees, sign nothing in" |
| `key_provider` | `"fs"` | `PROTON_PASS_KEY_PROVIDER` for that session: `fs` keeps the key beside it |
| `vault` | `""` | the vault a call that names none reads |
| `field` | `"password"` | the item field the vault read takes when its call names none |
| `mappings` | `[]` | `NAME=pass://vault/item/field` lines, the original's env map; `/protonpass env` reports them |
| `timeout_seconds` | `30` | one `pass-cli` call's ceiling |
| `cache_ttl_seconds` | `300` | how long a value stays in this image's memory; `0` reads every time |

## Use it

| Tool | What it reads |
| --- | --- |
| `protonpass_list` | the titles of one vault's items, and no value: how to find what a reference should name |
| `protonpass_get` | one field of one item: `pass://vault/item/field` (the `pass://` may be left off). The answer is the value alone |
| `protonpass_vault` | a whole vault, bulk-import style: every item whose title is a valid environment-variable name contributes its field, as `NAME=value` lines |
| `protonpass_status` | whether `pass-cli` runs and is signed in, the defaults, the cache, how many mappings |

A reference is exactly three segments, vault and item by name or by id (an id
is what tells two items of the same title apart), and one that is not is
refused before `pass-cli` runs. Values are cached in memory for
`cache_ttl_seconds`; pass `fresh` to read past it after a key was rotated.

`/protonpass` reports the session, `/protonpass env` the mappings as a dry
run (what a startup would export, never a value), `/protonpass get
pass://vault/item/field` reads one because you asked, `/protonpass login`
signs in with the token, `/protonpass forget` empties the cache. `(help
:protonpass)` carries the same guidance to the model.

Privacy: the cell opens no socket. Every read is one `pass-cli` call, and
`pass-cli` talks to Proton; its telemetry is turned off for these calls, and
each one carries a reason string (`Nodecode: load OPENAI_API_KEY`) that an
agent token's audit log records. A value the model reads is as sensitive as
the key it is: it is in the transcript once it is read.

## What the rewrite does not do

- **No startup environment injection.** The original was a Hermes secret
  source: it resolved `ENV_VAR → pass://…` before Hermes started. Nodecode has
  no secret-source seam, so the same reads are tools, and a mapping is a line
  `mappings` carries for `/protonpass env` rather than a variable that appears
  in the process. It follows that `set`, `remove`, `disable` and `sync` are the
  config and `/protonpass env`, not verbs of their own.
- **No on-disk value cache** and no file lock or 110-minute session marker:
  the cache is this image's memory only (the original's `protonpass_cache.json`
  held values in plaintext on disk), and a session that has died is logged in
  again on the first read that fails for it, once.
- **The child inherits this image's environment.** The original narrowed what
  `pass-cli` could see to an allowlist; `nlk:run-bounded` here lays the cell's
  variables over the process's instead. `pass-cli` opens no socket but its
  own, but a variable it is never told to look at is still in reach.
- **No `pass-cli` distribution, no version probe beyond `--version`**, and no
  Windows-specific session test.

## Test

```sh
.github/test-cell.sh tools/nodecode-protonpass
```

Eighteen tests, offline: every call goes through one stubbed seam, so the
argv a tool builds, the reference it accepts, the cache it keeps and the
string it answers are asserted with no `pass-cli`, no account and no network.

## License

MIT, Lucas Nicolas's: see `LICENSE` and `NOTICE`.
