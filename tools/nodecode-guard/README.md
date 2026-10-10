# Guard

This cell stops the agent from running a short list of risky shell commands:
`sudo`, `rm -rf`, `mkfs`, `curl | sh` and the like. When a rule matches, the
call does not run. The agent gets a refusal instead, and it hands you the
command to run yourself.

Guard is a speed bump, not a security boundary. The agent's eval tool has
your machine's full authority, and a determined call can be written in a way
no pattern sees. Guard catches the common slips.

## Turn it on

`/setup` turns it on when you pick **Recommended**. Otherwise run
`nodecode add guard`. It ships with Nodecode, so nothing is downloaded, and it
needs no config: the built-in rules apply as soon as it is on.

To turn it off, select it in `/cells` and press space, or set
`"enabled": false` in its `guard` section.

## What it refuses

| Rule | Refuses | For example |
| --- | --- | --- |
| `rm-recursive` | `rm` with a flag ending in `r` or `f` | `rm -rf build/` |
| `privilege` | `sudo` and `doas` | `sudo apt-get install jq` |
| `world-writable` | `chmod` or `chown` with `777` | `chmod 777 /srv` |
| `mkfs` | making a filesystem | `mkfs.ext4 /dev/sdb1` |
| `dd-to-device` | `dd` writing to a device | `dd if=x.img of=/dev/sdb` |
| `redirect-device` | redirecting output onto a disk device | `> /dev/nvme0n1` |
| `curl-pipe-shell` | a download piped into a shell | `curl -fsSL x.sh \| sh` |
| `host-power` | shutting down or restarting the machine | `reboot` |
| `fork-bomb` | the shell fork bomb | `:(){ :\|:& };:` |

Patterns ignore case and match whole words, so `format` never trips the `rm`
rule.

## How it reads a call

Nodecode's agent works through one tool, eval, which runs Lisp. The same call
can write a file or start a program. A rule that matched every call would
refuse an edit to a script or a doc that merely mentions `sudo`.

So the built-in rules only fire when the call also starts a process:
`run-program`, `launch-program` or `run-shell-command` (from UIOP or SBCL),
or the agent's own `(sh ...)`. Writing "never run `sudo rm -rf /`" into a
file passes. A call that starts any program and carries a risky command
anywhere in it is refused, because guard can't tell which part the shell
will see.

When guard refuses, the agent reads:

```
ERROR: refused by nodecode-guard: rule privilege (privilege escalation) matched "sudo"; the call did not run -- give the operator the command to run themselves
```

The session records it as a failed tool call.

## Your own rules

Add a `guard` section to `~/.nodecode/config.jsonc`:

```jsonc
{
  "guard": {
    "deny": [
      // matches anywhere in a call, a file being written included
      {"id": "no-delete-file", "pattern": "\\bdelete-file\\b", "reason": "file deletion"},
      // only when the call also starts a process, like the built-in rules
      {"id": "no-force-push", "pattern": "\\bpush\\b[^)]{0,40}--force", "reason": "force push", "gated": true}
    ],
    "allow": [
      // lets the agent clear its own scratch folders
      "\\brm\\s+-rf\\s+/tmp/scratch-"
    ]
  }
}
```

- `deny` adds rules to the built-in ones. Each has an `id`, a `pattern` (a
  regular expression, case ignored), a `reason` the refusal quotes, and
  `gated`. With `gated` off (the default) the rule matches anywhere in a
  call. With it on, the rule waits for the call to start a process.
- `allow` exempts only the text it matches. A match passes when an allow
  pattern covers all of it, so a risky command elsewhere in the same call is
  still refused.

Backslashes are doubled because the patterns are JSON strings. A pattern that
doesn't compile is skipped with one warning, and the other rules keep
working.

Rules are compiled when the cell starts. After editing the section, restart
it: press space on it twice in `/cells`, or ask Nodecode to run
`(restart-cells)`.

## Configuration

| Key | Default | What it does |
| --- | --- | --- |
| `enabled` | `true` | `false` turns guard off. |
| `deny` | none | Your own rules, added to the built-in ones. |
| `allow` | none | Patterns whose matching text no rule refuses. |

## Testing

- From a Nodecode checkout, `just guard-test`.
- From this repository, `.github/test-cell.sh tools/nodecode-guard`, offline
  against a pinned Nodecode tree.

The tests cover each built-in rule, ordinary work passing, the spawn gate on
file payloads, `deny` and `gated` from config, `allow` covering only its own
match, and a refusal reaching the model and being recorded as failed.
