# nodecode-memory-shield

Memory Shield: keeps the agent from rewriting or wiping what it remembers about you. A Nodecode rewrite of [churnast/hermes-memory-shield](https://github.com/churnast/hermes-memory-shield) by [churnast](https://github.com/churnast).

Nodecode keeps what it remembers as definitions: a memory about you is a
`(define-memory name "one sentence" :type :user)`, its own notes and lessons
are the other memory types and prose skills, and each one is a file in a
knowledge cell. One message can talk an agent into rewriting or wiping them,
and a group chat can talk it into writing something about you that is not
true. This cell judges every write before it runs.

## Get it

Tell the agent "add the memory shield cell", or run `nodecode add nc://memory-shield`.

## Use it

Nothing to set: the default policy is on as soon as the cell is. What it
allows, by default:

| Store | May be added to | May be corrected | May be deleted |
| --- | --- | --- | --- |
| your profile (a memory of `:type :user`) | anywhere | by you, in a direct chat | never |
| the agent's notes (every other memory and prose skill) | anywhere | anywhere | never |
| either, in a channel chat | only in a `trusted_sessions` session | never | never |

A write it refuses reaches the agent as a failed call, in words it can act
on: add a new dated entry instead of rewriting the old one, or ask you in a
direct chat. With `"mode": "observe"` nothing is refused; the write runs and
a notice says what the policy would have stopped, which is how to try the
policy on a busy week.

Type `/memory-shield` to read the policy, the knowledge folders it judges and
which session you are in.

## What it reads

The agent works through one tool, `eval`, and a call's form is read as data,
not grepped. What counts as a memory write:

- `(define-memory name "..." :type :user)` or `(define-skill name "...")` --
  an add, or a **correction** when the knowledge cells already hold that name.
- `(unintern 'name)` where that name is a definition the cells hold -- a
  deletion.
- `(edit path old new)` or `(write-file path text)` into a knowledge folder,
  judged as a correction, or as a deletion when the new text is empty or a
  placeholder such as `none`.
- A shell command that removes or moves one of those files.

A comment naming `define-memory`, a test string, or a write to any other file
never counts: the form is read, and only the memory writes in it are judged.

## Configuration

| Key | Default | What it does |
| --- | --- | --- |
| `profile` | `owner_edits` | `off`, `no_delete`, `owner_edits`, `append_only`, `read_only` for what is remembered about you. `owner_edits` allows a correction only in a direct chat. |
| `notes` | `no_delete` | The same levels for the agent's own notes and skills. |
| `shared` | `read_only` | Applied on top of both, in a channel chat. |
| `mode` | `block` | `block` refuses; `observe` lets the write run and says so. |
| `trusted_sessions` | none | Session ids judged like a direct chat: your own channel with the agent. |
| `folders` | none | Knowledge folders to judge. Empty asks the kernel for them. |

```jsonc
"memory-shield": {
  "profile": "owner_edits",
  "notes": "no_delete",
  "shared": "read_only",
  "mode": "block"
}
```

## Undo

A knowledge cell is a git repository and every keep is a commit, so a write
that got through is undone in the cell with git. The cell keeps no copies of
its own.

Memory Shield is a guard, not a security boundary: the agent's `eval` has the
machine's full authority, and a call written to hide its write from the reader
can pass. It catches the ordinary case, which is the one that happens.

## Testing

From this repository, offline against a pinned Nodecode tree:

```sh
.github/test-cell.sh tools/nodecode-memory-shield
```
