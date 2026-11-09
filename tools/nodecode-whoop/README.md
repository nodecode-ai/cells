# nodecode-whoop

WHOOP: your recovery, HRV, sleep and strain as five tools, read from WHOOP's own API. A Nodecode rewrite of [davidmosiah/whoop-mcp](https://github.com/davidmosiah/whoop-mcp) by [davidmosiah](https://github.com/davidmosiah).

The tools keep the original plugin's names and the data it reached:

| Tool | What it does |
| --- | --- |
| `whoop_daily_summary` | The headline of the last days: newest scored recovery (score, HRV, resting heart rate, SpO2, skin temperature), newest sleep (performance, efficiency, hours), newest cycle (strain, kilojoules, heart rate), the window's workouts and its averages. |
| `whoop_recovery_trend` | Recovery score, HRV and resting heart rate over a window, each with avg, min, max, a least-squares slope and the direction it spells. |
| `whoop_sleep_trend` | The same statistic for sleep performance, hours asleep and sleep efficiency. |
| `whoop_list_workouts` | The scored workouts of the last days, newest first: sport, strain, kilojoules, heart rate, distance. |
| `whoop_get_profile` | Which WHOOP account the token belongs to, and the body measurements WHOOP holds. |

Ask "how did I recover overnight?", "is my HRV trending up this month?", "was my sleep better before the trip?" — the model picks the tool and answers with the numbers and their dates.

## Get it

Tell the agent "add the whoop cell", or run `nodecode add nc://whoop`.

## Set it up

WHOOP has no public token you can just paste: each person makes their own WHOOP developer app, and that app's client id and secret turn into an OAuth refresh token. Two ways to get one:

1. **With the original connector's CLI**, which does the OAuth dance for you: `npx -y whoop-mcp-unofficial@0.7.0 setup --client generic`, then `auth`. It writes the tokens to `~/.whoop-mcp/tokens.json` and the client id and secret to `~/.whoop-mcp/config.json`.
2. **By hand**, if you would rather not run it: create an app at [developer.whoop.com](https://developer.whoop.com), add `http://127.0.0.1:3000/callback` as its redirect URL, and walk the authorization-code flow yourself.

Then, in `/settings` (or in the `whoop` section of `config.jsonc`):

```jsonc
"whoop": {
  "client_id": "your app's client id",
  "client_secret_env": "WHOOP_CLIENT_SECRET",   // or name a file, if you prefer
  "refresh_token_env": "WHOOP_REFRESH_TOKEN",   // or: "refresh_token_file": "~/.whoop-mcp/tokens.json"
  "timeout": 20
}
```

A secret is named, never written into the config: `client_secret_env` holds the name of the environment variable carrying the app's client secret (export `WHOOP_CLIENT_SECRET` in the shell that starts Nodecode), and the refresh token is named the same way or read from a file only you can read. Set exactly one of `refresh_token_env` and `refresh_token_file`.

The cell exchanges the refresh token for an access token itself, keeps that in memory for the process, and exchanges again when it expires. WHOOP rotates refresh tokens: when it hands a new one back and you named `refresh_token_file`, the cell writes it to that file (0600) so the next start uses it. With `refresh_token_env` the rotation is lost at restart, and if WHOOP invalidated the old token you run the connector's `auth` again.

| Setting | What it does |
| --- | --- |
| `api_base` | WHOOP's API root, `https://api.prod.whoop.com/developer` unless you point it elsewhere. |
| `token_url` | WHOOP's OAuth token endpoint. |
| `timeout` | How long one request may take, in seconds (default 20). |

## Use it

You do not call the tools; the model does, when a question is about your body's last days. A person can call the same functions from eval, in the `whoop` package:

```lisp
(whoop:daily-summary (nlk:make-json-object "days" 7))
(whoop:recovery-trend (nlk:make-json-object "days" 30))
```

`(help :whoop)` is the model's manual for the five verbs, kept while the cell runs.

### What it answers with

A trend is `{avg, min, max, slope, direction, n_valid}` per metric, where the slope is a least-squares fit over the scored records oldest to newest, against each record's own place in the window, and direction is `rising`, `falling`, `stable` or `insufficient_data`. A day WHOOP has not scored yet is a gap, not a zero, and the answers say so rather than averaging it in.

### What the rewrite leaves out

The original plugin is an MCP server: it runs as a child process, speaks JSON-RPC over stdio to an agent, and ships its own setup and auth commands, an optional local HTTP transport, a SQLite response cache, a shared wellness profile under `~/.delx-wellness/`, synthetic demo data, and more than twenty tools — one per collection and one per record by id. This cell is the five verbs the questions actually use: the same collections, the same scored records, the same trend statistic, called in-process instead of over stdio. The OAuth round trip the connector's CLI performed is not here (you bring a refresh token, or run its CLI once to get one), and neither is the demo data, the profile file, or `whoop_get_cycle`/`whoop_get_sleep`/`whoop_get_workout` by id.

## License

MIT. The tools, the endpoints and the trend statistic are davidmosiah's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
