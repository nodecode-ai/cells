# nodecode-permission-canvas

Permission Canvas: element-level permissions for desktop work — allow, read only, ask once, or hide. A Nodecode rewrite of [TANGBUDU/permission-canvas](https://github.com/TANGBUDU/permission-canvas) by [TANGBUDU](https://github.com/TANGBUDU).

The original wrapped Hermes's computer-use backend and enforced a policy of per-control rules on every capture and every click. Here the same policy engine is the cell: the model asks before it acts, and a person — never the model — writes the rules and grants one exact operation. Nothing leaves the machine: the policy, the approvals and the audit trail are three files in the folder you name.

| Level | What it means |
| --- | --- |
| `green` | Allow: the model may operate this control. |
| `blue` | Read only: observe it, but no click, drag, typing or value change. |
| `yellow` | Ask first: one exact action needs the person's approval, once. |
| `black` | Hide: removed from the observation, its rectangle masked, and no operation. |

## Get it

Tell the agent "add the permission-canvas cell", or run `nodecode add nc://permission-canvas`.

## Set it up

One setting is required, because the cell keeps its state in files:

```jsonc
{
  "permission-canvas": {
    "dir": "~/permission-canvas",        // policy.json, approvals.json, audit.jsonl
    "default_level": "yellow",           // what a control no rule matches gets
    "unidentified_level": "yellow",      // an unlabeled control in a restricted window
    "approval_ttl_seconds": 120,         // how long one approval waits
    "max_controls": 2000                 // how many controls one observation may carry
  }
}
```

Until `dir` is set the cell runs and its verbs refuse, naming the setting. A new folder starts with `default_level` ask-first, so nothing is allowed by accident.

## Use it

Three tools, one for each thing the model may do:

| Tool | What it answers |
| --- | --- |
| `permission_canvas_check` | One proposed action on one control: allowed, refused (read only or hidden), or one approval id the person must grant. |
| `permission_canvas_observe` | One window's controls: which are visible, which are withheld, and which rectangles to mask before the picture reaches the model. A withheld control is named by index only. |
| `permission_canvas_policy` | The policy in force: level, rules, selected window, pause, pending approvals. |

`/permission-canvas` is the person's side:

```
/permission-canvas status                     # the policy in force
/permission-canvas rules                      # every rule and its scope
/permission-canvas pending                    # what waits for you
/permission-canvas approve ID                 # allow that exact action, once
/permission-canvas deny ID                    # refuse it; the model is told not to retry
/permission-canvas rule hide role=Button label="Delete account"
/permission-canvas rule allow app=notes.exe role=Button label=Save
/permission-canvas remove r3
/permission-canvas default ask                 # green | blue | yellow | black, or allow/readonly/ask/hide
/permission-canvas unidentified ask
/permission-canvas pause | resume              # stop and start the agent's desktop access
/permission-canvas target 4242 7 notes.exe Notes   # only this window, until `target none`
/permission-canvas block app.exe / unblock app.exe
/permission-canvas hide-title "Secret"          # anything whose title or label matches is hidden
```

What it holds to:

- A rule matches on `app`, `role`, `label`, `label_regex`, `window_title_regex`, `pid` and `window_id`. The most restrictive match wins, and a scope field the tree did not report never rules a rule out — except for a green rule, which needs the evidence, so a missing field cannot widen what the model may operate.
- A control with no label, in a window that carries any restriction, gets the unidentified level rather than the default one.
- Something hidden takes its descendants with it, and anything overlapping its rectangle, and anything with no geometry to prove it lies clear of one.
- An approval is one operation: it names the action, the control, its arguments and a digest of the whole policy. It is used once, and editing the policy makes every earlier approval stale.
- Every decision is one line in `audit.jsonl`.

`(help :permission-canvas)` has the same, for the agent. `/permission-canvas` bare prints the status.

## What is in it

| File | What it is |
| --- | --- |
| `policy.lisp` | The levels, the rules, the matching, what an observation withholds, the approvals store and the folder. |
| `cell.lisp` | The three tools, `/permission-canvas`, the manual, and the config section. |
| `test/` | Offline tests: each level, one-use approval, withholding and masking, a rule typed at the shell, the pause, a selected window, a bad policy. |

## License

MIT, TANGBUDU's: see `LICENSE` and `NOTICE`.
