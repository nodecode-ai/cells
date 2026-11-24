# nodecode-git-review

Git Review: read every changed file of a repository as one scroll of text, then stage, commit, push and open a pull request. A Nodecode rewrite of [mister-ideas/git-review](https://github.com/mister-ideas/git-review) by [mister-ideas](https://github.com/mister-ideas).

The original is a review pane for Hermes Desktop: it stacks every changed file of
a repository in one continuous diff, next to the chat, and turns the lines a
person picks into citations for their next message. This cell keeps what the pane
computes and gives it to the model instead of a window: the same read scopes,
read one at a time as text the agent can quote, and the same write path.

## Get it

Tell the agent "add the git-review cell", or run `nodecode add nc://git-review`.

Nothing else to install: the cell runs the `git` you already have, in the
repository you name. `gh` (GitHub) or `glab` (GitLab) is needed only to open a
pull or merge request.

## Use it

Ask which changed:

- "Review what you just changed in the parser."
- "What did I leave uncommitted in this repo? Skip whitespace."
- "Show me the branch's net change against main, one file at a time."
- "Step through the three commits on this branch."

The model gets two tools:

- `git_review` — `repo` (a path; empty means the session's working directory),
  `scope` (`unstaged`, `staged`, `uncommitted` — the default —, `branch`,
  `commits`), `path` (one file or folder), `base`, `ref` (comma-separated commit
  ids, commits scope), `ignore_ws`, `files_only`. It answers the branch, one line
  a changed file with its `+`/`-` churn and its status, and the unified patch,
  with new untracked files rendered as their own hunks.
- `git_commit` — `message`, `paths` (empty = everything), `all`, `push`, `pr`. It
  stages, commits with hooks off, publishes a branch that has no upstream, and
  opens a pull or merge request with `gh pr create --fill` or `glab mr create
  --fill`.

`/review [scope] [path]` reads the same for the person typing it, and
`(git-review:review :repo "path" :scope "staged")` answers it from eval.

A patch longer than `max_chars` is cut on a line boundary with a note, so read one
scope or one path at a time. A new file over about four hundred lines is named and
cut.

## Settings

```jsonc
"git-review": {
  "enabled": true,
  "read_only": false,      // refuse stage, commit, push and pull requests
  "base": "",              // what branch and commits compare against; empty picks origin/HEAD, main or master
  "context_lines": 3,      // unchanged lines kept around each change
  "max_chars": 60000,      // the most patch text one call answers
  "new_files": true,       // show the content of new, untracked files
  "timeout_seconds": 20,   // one git command's ceiling
  "repos": []              // paths to fall back on when a session has no working directory
}
```

Then `(restart-cells "nodecode-git-review")`.

Committing, pushing and opening a pull request are the person's decisions: the
cell does them when asked and never rewrites history. `read_only` refuses all of
them in the backend.

## Left out

The Desktop pane itself: the split layout, syntax colours, folding of unchanged
runs, the file tree, drag-to-cite and the keyboard shortcut are drawing, and a
cell answers text. Also left out: the snapshot machinery behind the "Last agent
turn" scope (it records the working tree at every message, with an index and object
database of its own), the AI-drafted commit message (in Nodecode the agent writes
the message), and the dashboard routes the pane polls.

## License

MIT. The original plugin is Copyright (c) 2026 mister-ideas: see `LICENSE` and
`NOTICE`.
