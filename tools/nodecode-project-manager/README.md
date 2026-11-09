# nodecode-project-manager

Project Manager: every git repository under your own folders, with its branch, last commit and what is uncommitted. A Nodecode rewrite of [AiSolve-me/hermes-project-manager](https://github.com/AiSolve-me/hermes-project-manager) by [AiSolve-me](https://github.com/AiSolve-me).

Point it at the directories your repositories live in and ask what you have:

```
2 projects under /home/you/src:
  name  branch  last commit
  api  main  a1b2c3d 2026-11-01 09:30  fix the login flow  (2 changed)
  web  feature/api  77aa001 2026-10-20 08:00  add the api
```

It reads; it never writes. `git status`, `git log`, `git for-each-ref` and
`git rev-list` under each project answer the list, the detail, the branches and
the commits, and nothing is staged, committed, fetched or pushed.

## Get it

Tell the agent "add the project manager cell", or run `nodecode add nc://project-manager`.

## Set it up

```jsonc
{
  "project-manager": {
    "watch_paths": ["~/src", "~/work"],   // required: where your repositories are
    "depth": 2                              // how many levels below each one to look
  }
}
```

`watch_paths` is the one required key: a folder is a project when it holds a
`.git` (the folder of a normal clone, or the file a worktree keeps). Without
the key the cell refuses to start and `/cells` says so. The rest is optional:
`depth` (2), `max_projects` (50), `commits` (10), `branches` (25),
`changed_files` (20) and `cache_seconds` (15) bound one answer and how long a
scan is remembered.

## Use it

`/projects` lists every project and opens a picker with a row for each;
`/projects web` keeps what the search names.

The model has five tools: `list_projects` (with an optional `search`),
`get_project` (path, branch, distance from the upstream, last commit, changed
files, origin), `list_branches` (newest commit first, the checked-out one
marked), `get_recent_commits` (`limit`, `branch`) and `list_watch_paths`.
The same scans are Lisp functions in the `projects:` package, called through
eval:

```lisp
(projects:projects)                       ; every project, one line each
(projects:project "api")                  ; one project in full
(projects:branches "api")                 ; its local branches
(projects:commits "api" :limit 20 :branch "main")
(projects:watch-paths)                    ; the roots, and what each holds
```

`(help :projects)` is the model's manual. Every answer is a string; a refusal
is `ERROR: PROJECT-MANAGER-ERROR` with the reason.

## What it does not do

The original is a desktop dashboard: a FastAPI server, its browser UI, docker
stack status, worktree and staging screens, and push and pull buttons. This
cell keeps the scanning and the reading behind them, and leaves the writing to
the git you already run. See [NOTICE](NOTICE).
