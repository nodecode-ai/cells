# nodecode-calling-mom

Calling Mom: a cheap model answers when the work is stuck or about to do something risky, and only a rare escalation reaches an expensive one. A Nodecode rewrite of [SlyBase/calling-mom](https://github.com/SlyBase/calling-mom) by [SlyBase](https://github.com/SlyBase).

The model doing the work cannot tell when it is out of its depth, so the
cell does not ask it. It watches what the model *does* -- one tool call at a
time -- and asks a second, cheaper model about it:

| What it sees | What it asks |
| --- | --- |
| the identical call three times in ten | why the work is going in circles, and how to get out |
| the identical result three times in a row | the likely cause, and the next one to three steps |
| a command in the critical tier (`git push --force`, `terraform apply`, `gh pr merge`, `DROP TABLE`, `kubectl delete`, `rm -rf` of a broad folder, a secret write, a guard switched off) | the biggest risk in what just ran, and what to double-check |
| a command on another machine (`ssh`, `scp`, `pct`, `qm`) | one look, once |
| your own question, through `second_opinion` | a recommendation, the deciding reason, one risk |

Mom's answer is appended to the result of the call that provoked it, where
the working model reads it and keeps working. She can also say `NO_ACTION`,
and then nothing is said at all.

**The expensive tier is rare by construction.** Mom is asked first, and her
instruction says: normal answer, and only for a case she cannot judge as
safe from that context, answer exactly `ESCALATE: <reason>`. Only then is
olymp asked, and olymp has a hard limit in calls, not in dollars -- six a
day by default. A subscription never surprises you, and neither does an
invoice.

## Get it

Tell the agent "add the calling-mom cell", or run:

```sh
nodecode add nc://calling-mom
```

## Set it up

The cell works with no key of its own: with `mom` empty, every question
goes to the auxiliary model Nodecode already has configured, so it is
billed to that provider and nothing leaves the machine it would not
otherwise. Name a model to send the questions elsewhere.

```jsonc
{
  "calling-mom": {
    "assist": true,                     // watch tool calls; off = only the tools answer
    "mom": "openrouter/deepseek/deepseek-v4.1-flash",   // empty: the auxiliary model
    "olymp": "",                        // empty: no expensive tier at all
    "mom_calls_per_day": 20,
    "olymp_calls_per_day": 6,
    "cooldown_seconds": 60,             // one unsolicited question per minute, at most
    "commands_extra": []                // regexes of your own for the critical tier
  }
}
```

A target is `provider/model`, or a bare model on the auxiliary provider.
`assist` is the whole watcher switch: with it off, nothing is watched and
nothing is sent unless a tool is called.

Secrets are blanked before anything is sent -- `sk-`, `ghp_`, `xox*-`,
`AKIA...`, `bearer`/`token=`/`api_key=` values, and a password inside a URL.

## Use it

The model has two verbs:

| Tool | Answers |
| --- | --- |
| `second_opinion` | `question`, optional `context`: one short, budgeted question about a decision that is expensive to get wrong. |
| `risk_check` | `command`, optional `path`: which tier it falls in and the rule that matched -- it classifies, it blocks nothing. |

You have one command:

```
/calling-mom                 who is mom, who is olymp, today's calls
/calling-mom on | off        watch tool calls, or stop
/calling-mom ask QUESTION    ask mom one question now
/calling-mom mom NAME        the cheap advisor's model, for this run
/calling-mom olymp NAME      the expensive advisor, for this run
/calling-mom reset           the day's counters start over
```

and a manual page, `(help :calling-mom)`, which the model reads before it
reaches for either tool.

Advice warns; it cannot undo what already ran. It is a second opinion, not a
gate -- the cells that refuse a command are `guard` and `approvals`.
