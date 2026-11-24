# nodecode-archytan-lite

Archytan Lite: every tool call checked against a policy before it runs. A Nodecode rewrite of [High-ArchyTech-Solutions/hermes-archytan-lite](https://github.com/High-ArchyTech-Solutions/hermes-archytan-lite) by [High-ArchyTech-Solutions](https://github.com/High-ArchyTech-Solutions).

A call is not one thing. `(sh "git push origin main")` runs a program, and a
`(write-file "/etc/passwd" ...)` writes a file outside the workspace; a fetch
reaches a host. Archytan Lite reads the call, names each of those as an
*action* on a *resource*, and holds the whole call unless the operator's policy
grants every one of them. Nothing runs on a guess: no policy, a policy that
does not read, no workspace, or a command the form does not show (`$VAR`,
`$(...)`, a heredoc) is a refusal, not a pass.

## Get it

Tell the agent "add the archytan lite cell", or run `nodecode add nc://archytan-lite`.

## Set it up

Set the folder the agent may work in, then write a starter policy:

```jsonc
"archytan-lite": { "workspace": "/home/you/work" }
```

```
/archytan setup
```

That writes `~/.nodecode/archytan/policy.json`, granting ordinary work in the
workspace and little else: `ls`, `cat`, `grep`, git's read side (`status`, `diff`,
`log`, `show`, `branch`), writes under the workspace, and any tool call. Edit
the file and the next call follows it; no restart, no key, no network.

```jsonc
{
  "version": 1,
  "actions": [
    {"action": "file.read",  "grants": [{"resource_ids": ["/home/you/work/**"]}]},
    {"action": "file.write", "grants": [{"resource_ids": ["/home/you/work/**"]}],
     "assertions": {"max_per_window": 120, "window_seconds": 60}},
    {"action": "shell.run",  "grants": [{"resource_ids": ["ls", "git/status", "git/push"]}],
     "assertions": {"max_per_window": 60, "window_seconds": 60}},
    {"action": "shell.env",  "grants": [{"resource_ids": ["*"]}]},
    {"action": "web.fetch",  "grants": [{"resource_ids": ["https/github.com"]}]},
    {"action": "tool.call",  "grants": [{"resource_ids": ["*"]}]}
  ]
}
```

`policy` and `journal` name other files; `ungated` lists tool names that skip
the gate. A resource is a path, a program (`git/push` for a program with
subcommands), a host (`https/github.com`) or a tool name; `*` matches one path
segment, `**` the rest of one, and matching ignores case.

## Use it

With the cell on, every tool call goes through it. An ungranted action answers
the model with the refusal — the action, the resource, why, and that the call
did not run — and the transcript draws the call failed:

```
ERROR: refused by nodecode-archytan-lite: shell.run on "git/push" refused (the
policy grants no shell.run on "git/push"). The call did not run -- do not retry it
unchanged and do not work around it: tell the operator, who grants it in
~/.nodecode/archytan/policy.json.
```

From `eval`:

```lisp
(archytan:check "shell.run" "git/push")   ; => (:ALLOWED NIL :REASON "...")
(archytan:decisions :limit 20)            ; the last decisions, as journalled
```

`/archytan` says where the policy is and how the journal stands; `/archytan log
50` shows the last 50 decisions; `/archytan setup` writes a starter policy.

Every decision — allowed or refused — is appended to
`~/.nodecode/archytan/decisions.jsonl` with its timestamp, the tool, the action,
the resource and the reason.

## What it does not do

The original asks an out-of-process gate and keeps an Ed25519 receipt chain,
with a Docker-provisioned gate image, single-use capabilities and a signed
journal. A Nodecode cell runs in the organism, so this rewrite reads the policy
itself: what it keeps is the mapping (programs, paths, hosts, tools), the
all-or-nothing decision, the per-minute limits and the local journal. See
`NOTICE`.

## Test

Offline, no network: the policy file is read from a temporary directory and every HTTP
call is stubbed. From a cells checkout:

    .github/test-cell.sh tools/nodecode-archytan-lite <token>

The run covers start and stop, a call the policy grants and one it holds, the strings-carried
command, the window budget, the journal, the check verb, setup and the status command; its last
line reads:

    16 archytan-lite tests, 0 failed
