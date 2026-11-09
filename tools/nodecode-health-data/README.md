# nodecode-health-data

Health Data: a local store filled from Oura, and three tools that answer sleep, readiness, stress and food questions from it. A Nodecode rewrite of [Infinite-Labs-AI/hermes-health-apollo](https://github.com/Infinite-Labs-AI/hermes-health-apollo) by [Infinite-Labs-AI](https://github.com/Infinite-Labs-AI).

The tools are the original plugin's idea, narrowed to the half that needs no OAuth application of your own:

| Tool | What it does |
| --- | --- |
| `health_sync` | Fetches daily sleep, readiness, activity, stress and SpO2, and the long sleep period, from Oura into the store. |
| `health_query` | Reads the store: recent days, a date range, the most stressful days, a correlation between two metrics, coverage, or logged meals. |
| `health_log_food` | Logs one meal, with calories when you have them. |

Ask "how did I sleep this week?", "is my readiness worse on high-stress days?", "did the late dinners show up in my HRV?" — the model syncs when the data is stale, reads, and answers from your own baseline.

## Get it

Tell the agent "add the health-data cell", or run `nodecode add nc://health-data`.

## Set it up

The cell needs one thing: an Oura personal access token. Create one at Oura's developer site (Personal Access Tokens), and give it to the cell by reference — the name of an environment variable, or a file only you can read:

```jsonc
"healthdata": {
  "oura_token_env": "OURA_TOKEN",        // or: "oura_token_file": "~/.config/oura-token"
  "data_dir": "~/.nodecode/health-data", // optional; this is the default
  "days": 30,                             // optional; the default sync window
  "timeout": 30                           // optional; seconds per request
}
```

Then `/health sync` fetches the window and `/health` says what is stored.

Everything the tools answer is in `data_dir`: `daily.jsonl`, one row a day, and `food.jsonl`, one line a meal. Nothing is sent anywhere except Oura, and nothing is read from another application.

## Use it

You do not call the tools; the model does, when a question is about sleep, recovery, stress or food. A person can call the same functions from eval, in the `health-data` package:

```lisp
(health-data:query-now (nlk:make-json-object "query_type" "recent" "limit" 7))
```

`/health` prints the store's coverage in a line; `/health sync [days]` fetches.

`(help :health-data)` is the model's manual for the three verbs, kept while the cell runs: which query type answers which question, how to read a correlation, and the guardrails — cite dates and values, compare with your own baseline, never invent a calorie, never diagnose.

### What it leaves out

Oura is read with a personal access token; the original's browser OAuth flow, its six-hour background sync cron, its Google Health connector, its calendar peek through a Google Workspace helper, and its analysis-pack layer (`health_analyze` and friends) are not here. See `NOTICE`.

## License

MIT, the original's. The metrics, the query shapes and the coach's rules are Infinite-Labs-AI's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
