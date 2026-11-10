# nodecode-pushgate

Push gate: blocks `git push` and `gh pr create` until your check has passed on
exactly the content being pushed. A Nodecode rewrite of
[cirwel/hermes-receipts](https://github.com/cirwel/hermes-receipts) by
[cirwel](https://github.com/cirwel).

A receipt is one run of your check, bound to the Git tree your working
directory would commit to. Run the check, and the exit status is taken from
the process itself — the agent saying "tests passed" earns nothing. Edit a
file and the earlier result no longer qualifies for the changed content;
commit exactly what you checked and the receipt still applies, because the
tree id is the commit's own.

## Get it

Tell the agent "add the pushgate cell", or run `nodecode add nc://pushgate`.

## Set it up

Give it the command that must pass, in the `pushgate` section of
`~/.nodecode/config.jsonc` or on the Settings page:

```jsonc
{
  "pushgate": {
    "check": "npm test"
  }
}
```

That command is the check for every repository. One repository can have its
own instead: `/pushgate check make test` inside it, and `/pushgate check
--off` drops it again.

## Use it

```text
/pushgate                  the check, the current content and its receipt
/pushgate run              run the check now and record the result
/pushgate check <command>  set this repository's check
/pushgate log              the newest receipts
/pushgate clear            drop this repository's receipts
```

The agent has one tool, `pushgate`, with the actions `run`, `status`, `log`
and `clear`; only you change which command is the check. A typical turn:

```text
(sh "git push origin HEAD")   → refused: no passing receipt for that tree
pushgate {action: "run"}       → the check ran, passed, and is recorded
(sh "git push origin HEAD")   → allowed: the commit's tree carries a pass
```

A failed run for the same content supersedes an earlier pass. The refusal
names the files that changed since the last passing run, and how to earn one.

### What is gated

`git push` (with its source named, for example `git push origin HEAD` or
`git push origin feature:feature`) and `gh pr create`. A static `cd <dir> &&`
prefix, `git -C <dir>`, `env`/`sudo`-style wrappers and `sh -c` are followed
to the repository they name. `--all`, `--mirror`, `--tags`, `--follow-tags`,
dynamic or wildcard refspecs, `-c` configuration overrides, and commands
whose pushed commits cannot be read are refused rather than guessed at;
`--delete` and `--dry-run` are not gated. A repository with no check —
neither its own nor the section's — is never refused.

This is a workflow gate, not a security boundary: a push you run in your own
terminal, an alias, or a publish script is outside it. It does not prove the
check is sufficient, and it does not see ignored files, dependencies or
environment variables.

### How the fingerprint works

The tree id comes from Git itself: the working directory is staged in a
throwaway index (`GIT_INDEX_FILE`) inside this cell's data folder, and
`git write-tree` answers, so nothing of your index, HEAD or refs moves. Git
writes the blobs it hashed into the repository's object database, as any
`git add` would; they are unreferenced and harmless. A repository that cannot
be fingerprinted gets no new receipt, and the run says so.

### What it keeps

One JSON ledger in `pushgate/` under the Nodecode home: the check per
repository, and up to `max_receipts` receipts each with the tree, the command,
the status, the exit code and when it ran. No command output, and no network.

| Key | Default | What it does |
| --- | --- | --- |
| `check` | none | the command that must pass before a push |
| `max_receipts` | 200 | how many receipts to keep per repository |

## Testing

From this repository, offline against a pinned Nodecode tree:

```sh
.github/test-cell.sh tools/nodecode-pushgate
```

Git and the check are both stubbed, so the tests touch no repository and run
no process.
