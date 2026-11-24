# nodecode-staff

Staff: a roster of specialists the model hands one brief at a time, a board of the jobs it gave them, and an optional handoff to a coworker outside the process. A Nodecode rewrite of [leoresendedop-sys/grokbot-staff](https://github.com/leoresendedop-sys/grokbot-staff) by [leoresendedop-sys](https://github.com/leoresendedop-sys).

A specialist is instructions, not a process: one brief is one side call on your
own model, with that role's system prompt, and you get the answer back whole. No
session starts, no tool runs inside it, and nothing is remembered except the job
on the board. Six roles come built in -- a chief of staff, a SaaS analyst, a
design researcher, a vision-agent architect, a computer-vision researcher and a
social-conversation researcher -- and you can replace them with your own.

| | |
| --- | --- |
| `staff_assign` | One specialist answers one brief: `specialist`, `task`, and optional `title` and `context`. The job and its answer stay on the board. |
| `staff_status` | The roster, the recent jobs and how a handoff is configured; with a job id, that job whole. |
| `staff_delegate` | The brief handed to a coworker over your webhook and/or a shared folder. Answers an id at once. |
| `staff_collect` | The answer back: read from the shared folder, or pasted. |
| `staff_handoffs` | The handoffs, pending, done or failed. |
| `/staff` | The same five, typed: `/staff`, `/staff status [job_id]`, `/staff <specialist> <task>`, `/staff delegate <task>`, `/staff handoffs`, `/staff collect <job_id> [result]`. |

## Get it

Tell the agent "add the staff cell", or run `nodecode add nc://staff`.

## Set it up

Nothing to obtain: the specialists answer on the model the session is already
on, so there is no key to paste. The `staff` section of
`~/.nodecode/config.jsonc` (`/settings` in a shell, or the file) carries the
rest, and all of it is optional:

```jsonc
"staff": {
  "specialists": [
    { "id": "ops", "title": "Ops lead", "focus": "runs the week",
      "prompt": "You run operations. Give the plan as days, with what is due when." }
  ],
  "inbox_dir": "~/CoworkerInbox",          // a shared folder the coworker reads
  "webhook_url": "https://your.host/hook", // your own webhook routine, POSTed JSON
  "state_dir": "~/staff",                  // where the board is kept
  "webhook_timeout_seconds": 15,
  "max_output_tokens": 1200
}
```

A roster you write replaces the built-in team whole; an entry needs an `id` of
lower-case letters, digits, `-` and `_` (at most 40), and may carry `title`,
`focus` and `prompt`. The webhook URL may also come from the
`GROKBOT_STAFF_WEBHOOK_URL` environment variable, which the original plugin
read, so an existing setup keeps working. Without `state_dir` the board is one
file, `staff/jobs.json`, under the Nodecode home.

## Use it

```
staff_assign(specialist: "chief", task: "Ship a pricing page for three tiers by Friday.")
staff_assign(specialist: "saas", task: "Price three tiers against the two competitors in the thread.",
             context: "We sell to teams of 5-50; the CFO wants a floor of 30 EUR a seat.")
```

The first restates the goal as work items and names the specialist for each; the
second answers with the analyst's own reasoning. `staff_status` reads either
answer again, and `staff_status job_id=<id>` reads one whole.

Work that will outlive the turn -- research, something scheduled, a job that
needs a bot of its own -- goes out with `staff_delegate`:

```
staff_delegate(task: "Research the last two quarter's competitor launches and write a page.",
               specialist: "saas")
```

It answers an id at once. With `inbox_dir` set, the brief is written as
`handoffs/<id>.json` in that folder; with `webhook_url` set, it is POSTed there
as JSON (https only, plain http for a loopback host). `staff_collect` then reads
`results/<id>.json`, `.md` or `.txt` from the same folder -- a `.json` answers
its `result` or `text` member -- or takes the answer pasted as `result`. With
neither configured, a handoff says what to set and sends nothing, rather than
pretending to have sent it.

### What it does not do

It does not run a specialist as an agent: no tools, no session, no memory past
the brief. It does not retry a handoff, poll, or keep a background worker: a
handoff is one POST and/or one file, and you collect when the coworker has
answered. Nothing is sent anywhere but the webhook and folder you name, and the
status answer shows a configured webhook as `scheme://host/...`, so a token in
its path or query never lands in a transcript.

## License

MIT, the original's. The specialist personas and the bridge's rules are
leoresendedop-sys's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
