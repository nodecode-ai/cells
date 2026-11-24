# nodecode-caprover

CapRover Operator: plan an immutable image, have the operator approve it, deploy once, and reconcile. A Nodecode rewrite of [fmercurio/hermes-caprover-operator](https://github.com/fmercurio/hermes-caprover-operator) by [fmercurio](https://github.com/fmercurio).

It brings one bounded deployment workflow to one CapRover app you already run:
the model reads the app, saves a plan, and asks you to approve it; you type
`/caprover approve <plan id>` in the shell, and only then can `caprover_apply`
re-read the app, refuse a changed baseline, and make at most one deployment
request. If the answer is lost, the plan stays uncertain and `caprover_reconcile`
observes the app read-only — never a second deployment for the same plan.

## Get it

Tell the agent "add the caprover cell", or run `nodecode add nc://caprover`.

## Set it up

In `/settings` (or the config file), one section:

```jsonc
"caprover": {
  "url": "https://captain.example.com",   // the one server this cell may reach
  "app_name": "example-app",               // an app that already exists there
  "health_url": "https://example-app.example.com/health",  // optional
  "plan_ttl_seconds": 300,
  "token_env": "CAPROVER_API_TOKEN"        // or "token_file": "/path/to/token"
}
```

The token is CapRover's own API token, sent only as its `x-captain-auth`
header to the `url` you set. The cell reads no other credential file and talks
to no other host; `health_url` gets a separate unauthenticated GET. Ask the
agent to run the doctor first: it reports settings and app access without
creating a plan.

## Use it

| Tool | Argument | What it does |
| --- | --- | --- |
| `caprover_doctor` | none | Read-only setup, token and app check; no plan, no deployment. |
| `caprover_plan` | `image` | Saves a plan bound to the app's current version, image, replicas and definition fingerprint. The image must end in `@sha256:` and 64 lowercase hex characters. |
| `caprover_apply` | `plan_id` | Refused until you approve that plan id. Then one deployment request at most. |
| `caprover_reconcile` | `plan_id` | Read-only observation of the outcome, with a dated receipt. |
| `caprover_status` | `plan_id` (optional) | The local ledger. No server request. |

And the command: `/caprover` lists the saved plans, `/caprover show ID` shows
one, `/caprover approve ID` is your approval — one deployment attempt may
follow it, and only from the shell you typed it in.

What a receipt says:

| Status | Meaning |
| --- | --- |
| `planned` | Saved intent. Nothing deployed. |
| `consumed_drift` | The app changed after planning; no request was made. |
| `consumed_expired`, `expired` | The plan ran out; no request was made. |
| `consumed_preflight_failed` | The app could not be re-read; no request was made. |
| `reconcile_required` | A request may have happened: reconcile this plan. |
| `deployment_recorded` | The app's metadata records the requested image; no health evidence. |
| `metadata_http_observed` | That, and the health URL answered 2xx. The serving image is not proven. |
| `observed_unhealthy` | The metadata matches, but health or replica evidence contradicts it. |
| `superseded` | A later, different image was deployed on top. |

Plans, receipts and approvals are JSON files under `~/.nodecode/caprover/`.
Reconcile an uncertain plan; never re-apply it. Deploying an image runs its
startup behavior, and returning to an older image restores no environment
values, volumes or data — the cell performs no rollback.

Run `.github/test-cell.sh tools/nodecode-caprover` from the cells repository:
ten offline tests over a stubbed CapRover server.
