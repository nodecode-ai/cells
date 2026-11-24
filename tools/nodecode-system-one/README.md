# nodecode-system-one

Local System One: ask a local sidecar whether a task needs the public web, whether it
is a fast or a strong-tier task, and whether a finished event should interrupt the
user. A Nodecode rewrite of
[NMTZ-z/system-one-benchmark-lab](https://github.com/NMTZ-z/system-one-benchmark-lab)
by [NMTZ-z](https://github.com/NMTZ-z).

Three small classification gates, answered by a service the operator runs on their
own machine, so nothing about the turn leaves the box. The model gets two tools,
`system_one_route` (both pre-turn gates, one call) and `system_one_notify` (one
finished event through the Notification Gate), and every gate is a Lisp function
an eval form can call too: `(system-one:route "…")`. `/system-one` is the
operator's way in, and `(help :system-one)` is the model's page.

Every gate fails open. A sidecar that is not running, is slow, or answers nothing
readable is `undecided`, and the caller decides as it would have without the cell.

## Get it

Tell the agent "add the system-one cell", or run `nodecode add nc://system-one`.

## Set it up

Run the sidecar first: it is the `local_system_one` service of the same
repository, a separate install, and on an Apple-silicon Mac it answers with a
small local model. Start it on its loopback port, and the defaults of this cell
find it. Nothing else is required.

```jsonc
{
  "system-one": {
    // where the sidecar answers; the default is its loopback port
    "service_url": "http://127.0.0.1:8787",
    // one gate's HTTP ceiling; a slow gate fails open
    "timeout_ms": 500,
    // only when the sidecar was started with one
    "auth_token_env": "LOCAL_SYSTEM_ONE_TOKEN"
  }
}
```

Each gate can be switched off on its own (`search_gate`, `model_tier_gate`,
`notification_gate`), and `canary_advice` turns off the `canary:` line described
below.

## Use it

```lisp
(system-one:route "review the local git diff and summarize the changes")
;; search: no_search (rule, local_file_or_repo) - no public web needed
;;   canary: an audited hard no-Web rule -- in acknowledged Canary mode the original
;;   hides the web tools on the turn's first provider call
;; model tier: fast (rule, bounded_transform; p_strong 0.080) - a fast, cheap tier is enough

(system-one:notify-gate "the migration finished with three files to review")
;; notification: digest (model; priority 2.305) - add it to a digest, do not interrupt

(system-one:health)
;; health: answers (service ok), neural engine healthy
```

The two tools say the same thing to the model. `/system-one TASK` asks about a
task from a shell, and `/system-one` alone says where the cell points and whether
the sidecar answers.

The task text and the context passed beside it go to the sidecar and nowhere
else; it is a classifier on the same machine, and no telemetry rides with the
call.

## What is not carried over

The original hooks a Hermes turn, so in its acknowledged Canary mode it can
rewrite the provider request: hide `web_search` and `web_extract` on the first
call of a turn when an audited hard no-Web rule fires, and lower
`reasoning_effort` for one `gemini-3.8-flash-tiered` request when an audited
hard-fast rule fires. Nodecode has no provider-request seam a cell can edit, so
the same rules are reported as advice on the decision's own line (`canary: …`)
and nothing is rewritten. The reasoning downgrade is the original's off-by-default
experiment; its 32-pair benchmark did not show a reliable speedup.

Also left out: the Hermes-only task enrichment that reads the owned kanban card
or strips the cron wrapper from a scheduled prompt (Nodecode has neither), the
plugin's per-turn mode switch (`off`, `shadow`, `canary`) because a cell is
asked rather than hooked, and its persisted shadow history.

Apache-2.0. See [NOTICE](NOTICE).
