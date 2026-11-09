# nodecode-diff-review

Diff Review: reviews a git diff and flags debug prints, hardcoded secrets, bare excepts and TODO markers on the lines it adds. A Nodecode rewrite of [sprmn24/hermes-plugin-diff-review](https://github.com/sprmn24/hermes-plugin-diff-review) by [sprmn24](https://github.com/sprmn24).

The original was one Python tool over `git diff`. This cell runs the same git
commands and keeps the same three modes and the same JSON answer, in Lisp,
with nothing bundled: no Python, no dependencies beyond the image.

## Get it

Tell the agent "add the diff-review cell", or run:

```sh
nodecode add nc://diff-review
```

## Set it up

Nothing is required. `git` 2.28 or newer has to be on `PATH`, and the review
reads the working tree of this process's own directory when the section says
nothing. All of it is optional, in `~/.nodecode/config.jsonc`:

```jsonc
{
  "diff-review": {
    "repo": "/work/project",   // absolute; the directory to review when a call names none
    "timeout_seconds": 30,     // one git run's ceiling; its whole process tree is killed past it
    "max_warnings": 200,       // how many warnings one answer lists
    "max_files": 500            // how many files one answer lists
  }
}
```

The counts say how many there really were when a list is cut. `/settings` and
the Settings tab write the same section.

## Use it

The model calls one tool:

```
diff_review()                          # the working tree against HEAD: staged, unstaged, untracked
diff_review(target: "origin/main")     # the commits this branch adds, like a pull request's diff
diff_review(base: "v1.2.0")            # the working tree against an older commit
diff_review(diff_text: "...")          # a unified diff you already have
path: "/work/project"                  # which repository, when it is not this one
```

and gets the original's JSON back: `status` (`ok`, `no_diff` or `error` with
`message` and `error_type`), `files_changed`, `total_added`, `total_removed`,
`files` (path, added, removed, status), `warnings` (file, line, issue),
`untracked_files`, `comparison` and `notes`.

On added lines only, it reports:

| What it finds | Label |
| --- | --- |
| `print(`, `pprint(` | debug print statement |
| `console.log(`, `.debug(`, `.info(`, `.warn(`, `.error(`, `.trace(` | console.log statement (the method it saw) |
| `token = "..."`, `api_key: '...'`, `SECRET=...` in .env or YAML form | possible hardcoded secret |
| `except:` | bare except clause |
| `except Exception`, `except (A, B)` | broad Exception catch |
| `TODO`, `FIXME`, `HACK`, `XXX` | unresolved TODO/FIXME |

The operator reads the same review with `/diff-review`, or `/diff-review
origin/main` for the branch's own commits, as a few lines instead of JSON. The
model reads the whole contract with `(help :diff-review)`.

What it does not do: it never writes, changes nothing in the repository, and
sends nothing anywhere. It reads the untracked files it finds (regular files
only, at most 1 MiB each, 200 files and 5 MiB per review) and reads nothing
else on the machine. A credential-shaped run of text in a message it answers
is masked.

## Test it

```sh
.github/test-cell.sh tools/nodecode-diff-review
```

The run needs no repository, no git install and no network: every git command
line goes through a stub that records the argv and answers what a repository
would.
