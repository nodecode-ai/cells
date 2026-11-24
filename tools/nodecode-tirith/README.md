# nodecode-tirith

Tirith: every shell command the agent runs is checked by your own tirith before it runs. A Nodecode rewrite of [sheeki03/hermes-plugin-tirith](https://github.com/sheeki03/hermes-plugin-tirith) by [sheeki03](https://github.com/sheeki03).

[tirith](https://github.com/sheeki03/tirith) is a terminal security scanner: lookalike
and homograph URLs, a download piped into a shell, obfuscated execution, credentials
sent to a paste site, a malicious package, terminal injection. This cell puts it in
front of the agent's shell: an eval form's `(sh ...)` and the other spawn calls are
read out of the form, the command goes to `tirith check` on standard input, and what
tirith says decides whether the call runs, runs with a warning on its result, or does
not run at all.

The cell is the seam, not the scanner: it never downloads, installs or updates tirith,
and makes no network request of its own.

## Get it

Tell the agent "add the tirith cell", or run `nodecode add nc://tirith`.

## Set it up

Install tirith 0.5.0 or later yourself first, and check it answers:

```
brew install tirith        # or: npm install -g tirith / cargo install tirith
tirith --version
```

Then leave the section empty: the cell finds `tirith` on `PATH`. Nothing else has to
be set. The section changes the posture:

- `tirith.path` — the executable, if it is not on `PATH`. Empty finds it there.
- `tirith.block_action` — `refuse` (the default) or `ask`: a blocked command is
  refused, or the person at the page is asked Allow once or Deny.
- `tirith.fail_closed` — on by default: a command tirith could not check (no binary,
  older than 0.5.0, a timeout, no verdict) is refused too. Off runs it unchecked.
- `tirith.warn_action` — `allow` (the default: the command runs and tirith's warning
  is appended to the result the agent reads) or `refuse`.
- `tirith.min_severity` and `tirith.ignore_rules` — which ordinary warnings are left
  out. A CRITICAL finding and a guarded rule (obfuscation, pipe-to-shell, terminal
  injection, a known-malicious package) always show.
- `tirith.offline` — checks run with `TIRITH_OFFLINE=1`, so tirith's package and URL
  lookups use its local cache only.
- `tirith.timeout` — seconds one check may take (10 by default).

```jsonc
"tirith": { "block_action": "ask", "min_severity": "medium", "timeout": 8 }
```

A tirith policy you already have applies unchanged: the cell runs the same
`tirith check --json --non-interactive --shell posix` the scanner's own docs describe.

## Use it

A command is checked as it runs; nothing to type. When a check blocks a command, the
agent is told what fired and not to retry it.

- `/tirith` reports the binary it found and the posture in force; `/tirith rm -rf /`
  checks one command and answers the verdict in words.
- `tirith_check` is the same for the model, so it can ask before writing a command
  into a form.
- `(help :tirith)` is the manual topic the agent reads.

## Licence

MIT, the original's. See `LICENSE` and `NOTICE`.
