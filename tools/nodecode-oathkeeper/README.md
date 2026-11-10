# nodecode-oathkeeper

Oathkeeper: an owner's oath of never/require rules, enforced deterministically on every tool call. A Nodecode rewrite of [Whydiriki700/oathkeeper](https://github.com/Whydiriki700/oathkeeper) by [Whydiriki700](https://github.com/Whydiriki700).

Prompt instructions are advice: an agent can forget them, a compaction can lose
them, and a confident model can talk itself into an exception. An oath is the
short list of things you say must never happen, and this cell enforces it at
the tool boundary -- no model, no network, no goodwill.

## Get it

Tell the agent "add the oathkeeper cell", or run `nodecode add nc://oathkeeper`.

## Set it up

There is nothing to configure: write the oath once in
`~/.nodecode/oathkeeper/oath.txt`. Enforcement starts the moment the file
exists, and until then the cell lets every call through.

```
# my agent's oath
standing mission: keep the owner's work moving without being asked twice
never delete: ~/notes/**
never modify: ~/.nodecode/cells/**
always require backup before modify: ~/notes/**
```

Three line kinds:

- `standing mission: <text>` is briefed in the cell's standing notice, which
  the model reads at the tail of every round's prompt.
- `never <verb>: <pattern>` refuses a call whose intended change matches, and
  the refusal quotes the line that was broken.
- `always require <evidence> before <verb>: <pattern>` refuses until evidence
  of that kind was recorded earlier in the same session -- for example a copy
  made with `cp`, `rsync`, `tar`, `zip` or `scp` of the same tree.

Verbs are `delete`, `modify`, `create` and `any`. A pattern ending in `/**`
covers a whole tree, a pattern carrying `*`, `?` or `[` is a glob, and any
other pattern is the path itself or anything under it. `~`, `$HOME` and
`$NODECODE_HOME` expand. `#` starts a comment; a line that does not parse is
reported by `oath_status` and ignored.

The oath file is protected by the cell itself: the agent cannot modify or
delete it through any call, and no tool is registered that could write,
weaken or delete it. Changing the oath is your act, made outside the agent.

A cell may name another file, and turn the snapshot reporting off:

```jsonc
"oathkeeper": { "oath_file": "~/my-oath.txt", "watch_changes": true }
```

## Use it

Ask `/oathkeeper` and it answers with the oath in force and the ledger's
counts. The agent has two tools: `oath_status` (the missions, the rules, the
parse errors and the ledger's counts) and `oath_check`, which dry-checks one
hypothetical call without running it.

A refused call does not run, and the agent reads why:

```
ERROR: oathkeeper blocked this tool call: it would break the owner's oath.
- oath line 3 forbids delete on this target. Rule: "never delete: ~/notes/**".
  Action: delete /home/you/notes/todo.md.
```

Beyond the gate, the cell keeps a fingerprint of every file the oath's
patterns cover and reports, on the next call, whatever moved without passing
through it: an edit by another process, an edit by you, a second agent.

## Configuration

| Key | Default | What it does |
| --- | --- | --- |
| `oath_file` | `~/.nodecode/oathkeeper/oath.txt` | the oath this cell enforces |
| `watch_changes` | `true` | report files under the oath's patterns that changed outside an approved call |
| `hash_limit_bytes` | `1000000` | files up to this size are fingerprinted by content, larger ones by size and time |
| `max_files` | `5000` | how many files one pattern's fingerprint covers |

## Honest limits

- A call is read, not grepped: the spawns in an eval form (`sh`,
  `run-program`) and the file verbs it calls (`delete-file`, `write-file`,
  `edit`, `rename-file`, `copy-file`) give the intended changes, and a shell
  command's first 4096 characters are scanned. Inline interpreter code
  (`python -c`, a heredoc, a script) is not parsed; the fingerprint snapshot
  is the second line of defence there.
- Files above `hash_limit_bytes` are fingerprinted by size and time, so a
  same-size swap within one second would not be seen.
- Snapshots are compared at the head of each call, so a change made and
  reverted between two calls leaves no trace.
- Evidence is session-scoped: a backup made in an earlier session does not
  satisfy an `always require` line in this one.
- This is a speed bump the agent cannot argue with, not a sandbox: the agent's
  eval tool still has your machine's authority.

## License

MIT, the original's: see `LICENSE` and `NOTICE`. The oath file format, the
rules and the plugin are Whydiriki700's; the Lisp is a rewrite.
