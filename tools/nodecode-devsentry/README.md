# nodecode-devsentry

DevSentry: read-only project inventory, traceback leads and tracked Git diffs. A Nodecode rewrite of [Archangel20/devsentry](https://github.com/Archangel20/devsentry) by [Archangel20](https://github.com/Archangel20).

The original was three Python tools for a coding agent: one inventoried a
folder, one read a pasted traceback, one read `git diff`. This cell registers
the same three tool names with the same arguments, and answers the same JSON
in Lisp, with nothing bundled and no dependency beyond the image.

Nothing it does changes your project. It does not open file contents to
inventory a folder, does not follow symlinks, does not execute the project,
and does not send anything anywhere: no HTTP client, no telemetry.

## Get it

Tell the agent "add the devsentry cell", or run:

```sh
nodecode add nc://devsentry
```

## Use it

Ask in the conversation, and the model calls one of the three tools:

```
what is in ~/src/api, and which languages does it use?
here is my traceback: <paste>  -- what does it point at?
review my tracked changes before I commit
```

`inspect_project(path)` lists the entries of a folder, counts files by
extension into approximate language totals, and names the dependency, build,
cache and version-control folders it did not enter. `diagnose_error(trace,
project_path?)` reads Python, JavaScript, Java, .NET, Go and Rust frames and
exception names, adds a lead a person can check for each one, and — when you
name a project — quotes the lines around a cited line, but only for a file
that stays inside that project and is not a symlink. `review_changes(path)`
reports staged and unstaged tracked changes as separate scopes, with each
file's patch; untracked files are counted and not read, and a path that looks
like a credential carries no patch at all.

The operator reads the same inventory whole:

```
/devsentry            this folder
/devsentry PATH       the folder you name
```

and the manual is `(help :devsentry)`.

## Settings

Every setting is a default a call may override, so there is nothing to fill
in before using it. In the `devsentry` section of `~/.nodecode/config.jsonc`
(or `/settings`): `max_depth` (6) and `max_entries` (500) bound an
inventory, `context_lines` (2) bounds the quoted source lines and the diff
context, `max_patch_bytes` (20000) bounds each file's patch, and
`git_timeout_seconds` (20) bounds one git run. A value outside the bound a
tool accepts is clamped; a value the section cannot read refuses the cell at
start with a notice naming the key.

## Notes

- Git runs as an argument list, never through a shell. Aliases, hooks, an
  external diff driver, textconv and fsmonitor from the repository are
  overridden for the run, and the index is hashed before and after so the
  answer can say it did not move.
- Language totals are counts by extension and a few well-known filenames,
  not a parse: the file is never opened.
- A diagnosis says `certainty: leads_only`. The leads are things to verify,
  not a confirmed root cause.

Run `.github/test-cell.sh tools/nodecode-devsentry` from the cells repository.
