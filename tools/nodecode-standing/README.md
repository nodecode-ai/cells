# nodecode-standing

Standing: earned trust for the agent's calls. A Nodecode rewrite of [percyhendrux123-prog/hermes-standing](https://github.com/percyhendrux123-prog/hermes-standing) by [percyhendrux123-prog](https://github.com/percyhendrux123-prog).

An actor has a level. The level decides what runs without asking; a check the
cell ran itself is what raises it. What it lets through without asking the
operator:

| Level | Also allowed |
| --- | --- |
| L0 | reads and computations |
| L1 | writes whose path resolves inside the workspace |
| L2 | shells, sends, spins, and a call it cannot read |
| L3 | anything; spending and deletion still ask below it |

A call below the level it needs puts a question up — Allow once, or Deny — and
runs only on Allow once. Nobody there to ask is the same refusal.

## Get it

Tell the agent "add the standing cell", or run `nodecode add nc://standing`.

## Set it up

Nothing is required: the cell starts at L1, and `/standing` and the log work
from the first run. To make it earn its keep, set in `~/.nodecode/config.jsonc`
(or in `/settings`, on the Settings tab) the members it reads:

```jsonc
{
  "standing": {
    "workspace": "/absolute/path/to/your/work",
    "file_proof_roots": ["/absolute/path/to/artifacts"],
    "http_proof_targets": ["http://127.0.0.1:8000/health"],
    "command_allowlist": ["git status --short"],
    "promote_after": 10,
    "max_auto_level": "L2"
  }
}
```

`workspace` is the folder whose writes count as local. `file_proof_roots` is
what a file proof may read (empty uses the workspace; with neither, no file is
read, and a credential store is never read). `http_proof_targets` are URL
prefixes an http proof may fetch, a trailing slash allowing a subtree; empty
refuses every fetch. `command_allowlist` holds exact read-only command lines a
command proof may run. `promote_after` is how many passing checks in a row
raise the level by one, up to `max_auto_level`; only `/standing set` gives L3.
`config.example.jsonc` carries every member, commented.

## Use it

Ask the agent to prove what it claims:

```
standing_proof(kind: "file",    target: "/work/build/app.zip", expect: "3f9a…c1")   ; a sha256, or a text inside
standing_proof(kind: "http",    target: "http://127.0.0.1:8000/health", expect: "ok")
standing_proof(kind: "command", target: "git status --short", expect: "")            ; one allowlisted line
```

The cell runs the check itself — it never takes the model's word for a result —
and the record counts it: a pass earns one point and a proof handle, enough
passes raise the level by one, and a completed check that does not match lowers
it by one. A check that could not run (no roots, a missing file, a command
nobody allowlisted) earns nothing and costs nothing. Do not put secrets in a
target or an expected result: the record keeps a hash of the check, not its
text.

The operator's own word:

```
/standing                                  the table: actor, level, passes, fails, last event
/standing log default                     the last twenty events
/standing set default L3 the owner said so sets a level, and only here
```

Model-facing functions, through eval: `(standing:proof "file" "/work/out.txt" "done")`,
`(standing:status)`. `(help :standing)` carries the same text while the cell
runs.

The record lives in the nodecode home (`standing/ledger.sexp`) unless `ledger`
names another file. It holds one row per actor and the events, and nothing of a
call's arguments beyond a hash.

## What it does not do

The original gates Hermes's own tool names and blocks a kanban card's
completion without a proof. Nodecode has one tool, `eval`, so this rewrite reads
the *form* a call evaluates — the command its `sh` runs, the path a write names
— and gates on that; and the completion gate becomes `proof_before`, a list of
tool names that need a passing proof in the session first.
