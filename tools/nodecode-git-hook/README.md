# nodecode-git-hook

Git Hook: pulls each clean worktree once before a read, then commits and pushes only the files each turn changed. A Nodecode rewrite of [aean0x/hermes-git-hook](https://github.com/aean0x/hermes-git-hook) by [aean0x](https://github.com/aean0x).

The original is a hook-only plugin for Hermes Agent: before a tool call it pulls the worktree the call touches, after it records which paths changed, and at the end of a turn it commits and pushes exactly those. This cell keeps that contract and its two safety rules — never stage a secret-looking name, never commit on a branch whose direct push the remote refuses — and runs on Nodecode's own hook points: `:TOOL` for the before and after of each call, `:ROUND` for the turn's end.

## Get it

Tell the agent "add the git-hook cell", or run `nodecode add nc://git-hook`.

Nothing to install: it runs the `git` you already have, in the repositories you name.

## Set it up

`roots` is required, and it is the whole safety story: a worktree outside every entry is never pulled, committed or pushed. In the `git-hook` section of `~/.nodecode/config.jsonc`:

```jsonc
"git-hook": {
  "roots": ["/home/you/projects"],
  "pull": true,          // pull --ff-only before a read, once per worktree per session
  "commit": true,        // commit the files a turn changed
  "push": true,          // push them to the branch's own upstream
  "message": "",         // empty writes "update <file>" or "update N files"
  "secret_names": [],    // extra file-name prefixes never staged
  "pull_timeout_seconds": 12,
  "push_timeout_seconds": 20,
  "budget_seconds": 20   // the whole before-a-call phase, across every worktree
}
```

`config.example.jsonc` in this folder is the same section, commented.

## Use it

Ask for work in a repository under `roots`, and the cell does its housekeeping around it:

- "Fix the failing test in the parser." — before the call that reads a file, its worktree is pulled if it is clean and has an upstream; after the turn, the files the turn changed are committed and pushed.
- "Why is the build broken?" — the read is on current code, not a stale checkout.

Nothing to call, unless you want to:

- `/git-hook` or `/git-hook status` — the switches, the allowlist, and what is waiting in each worktree.
- `/git-hook flush` — commit and push now instead of at the turn's end.
- `/git-hook off` / `/git-hook on` — stop and start committing.
- `/git-hook push off` / `/git-hook push on` — keep commits local.

`(help :git-hook)` is the same, for the model.

What it will not do:

- Touch a worktree outside `roots`.
- Run a repository's own `core.fsmonitor` command, or ask a terminal question: every git run is `-c core.fsmonitor=`, `GIT_TERMINAL_PROMPT=0`, with a bound.
- Stage `.env*`, `auth.json`, `credentials*`, `id_rsa*` and friends, `*.pem`, `*.key`, or anything `secret_names` names.
- Sweep up dirt the turn did not make: an edit counts only when the path's status, size or write date moved since the last look, so a file edited while already dirty is committed and a file nobody touched is left alone.
- Keep a commit a branch can never push: when the remote answers with a policy refusal (GH013, a protected branch, a pre-receive check), it drops its own commit with `reset --soft`, leaves the change in the tree, says so once, and stops committing on that branch for the session.
- Fail a tool call: a git that cannot run, a deleted directory or a timeout leaves the turn unsynced and says nothing.

## Limits

- The delta's signature is status, size and write date, as the host's own staleness memos use; the original folded in `mtime_ns`.
- The "this branch refuses direct pushes" memo lasts for the session, not on disk, so a removed ruleset heals at the next start.
- Pulling happens on the first read of a worktree in a session, not once per tool call, matching the original.
