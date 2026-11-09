# nodecode-actvox

ActVox Inbox: what waits for you in an ActVox workspace - the tasks assigned to you and the comments that @mention you - read through your own ActVox CLI. A Nodecode rewrite of [ActVox/actvox-plugins](https://github.com/ActVox/actvox-plugins) by [ActVox](https://github.com/ActVox).

## Get it

Tell the agent "add the actvox cell", or run `nodecode add nc://actvox`.

## Set it up

Install and sign in to the CLI the cell reads with:

```sh
npm i -g @actvox/cli
actvox auth login
```

Then turn the `actvox` cell on. Its section in the settings takes:

| Setting | What it is |
| --- | --- |
| `cli_path` | the `actvox` binary; empty finds it on PATH |
| `profile` | the CLI login profile to read with; empty uses the CLI's default |
| `note` | open a turn with one line when tasks or mentions wait (default on) |
| `cache_seconds` | how long that line reuses one inbox read (default 60) |
| `timeout_seconds` | one CLI read's ceiling (default 15; the turn's line uses 5) |

The cell holds no token: it runs the CLI you signed in, with your login.

## Use it

- `/actvox inbox` lists what waits, a workspace a block, each task by id and state; `/actvox` alone answers the same with a panel, and `/actvox status` shows the CLI, the login and how many workspaces it reaches.
- The model calls `actvox_inbox` for the same list, optionally for one `workspace_id`, and a turn's first request carries one line when something waits: "ActVox: 1 task and 1 mention wait for you."
- `(actvox:inbox)` and `(actvox:status)` answer the same two reports from eval.

It reads only: claim, comment and submit in ActVox. A claimed task whose lease is still alive counts as in progress, and a workspace that takes no tasks, or refused, is named rather than listed.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the section, the CLI reader, the inbox report, the `:ROUND` note, the tool and `/actvox` |
| `test/cell-test.lisp` | the tests, every CLI call stubbed |

## License

MIT, Copyright (c) 2026 ActVox: see `LICENSE` and `NOTICE`.
