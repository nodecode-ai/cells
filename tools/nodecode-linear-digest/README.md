# nodecode-linear-digest

Linear Digest: the open Linear issues assigned to you, grouped by overdue, due today and gone quiet, with no model call. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-linear-digest](https://github.com/TakeshiTGAL/hermes-plugin-linear-digest) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the linear-digest cell", or run `nodecode add nc://linear-digest`.

## Set it up

The cell reads Linear with a personal API key, and nothing else.

1. In Linear, open **Settings**, **Security and access**, and create a personal API key.
2. In `/settings`, open the `linear-digest` section and put the key in `api_key`, or name the environment variable that holds it in `api_key_env` (`LINEAR_API_KEY` by default).

`/linear-digest status` says, in words, where the key came from, and never prints it.

## Use it

| What | What it answers |
| --- | --- |
| `linear_digest` (tool) | the list: overdue, due today, not updated for N days, then the rest, each issue's identifier, title, state, due date and URL |
| `/linear-digest` | the same list, for the operator reading the shell |
| `/linear-digest status` | how the cell is set up: the key's source, the quiet window, the line cap |

The list is one read-only query to `api.linear.app`, at most ten pages of fifty, and a run sends the key as the `Authorization` value with no `Bearer` prefix. Nothing is created, edited or commented on; archived issues and the closed workflow types are left out. A group with nothing in it is dropped, its heading carries its count, a blank line separates groups, and a line is the identifier, title, state, due date and URL separated by tabs. At most `show_limit` (30) issue lines are listed, then `+N more`; a page that was not read says so rather than pretending the list is complete. An empty answer means no open issue is assigned to this key.

`stale_days` (7 by default) is the quiet window: an issue Linear has not updated for that long, and that is not already overdue or due today, is listed under `Not updated for N days`. `timeout_seconds` bounds one request.

To have it arrive every morning without asking, put it on a clock with the [cron](https://github.com/nodecode-ai/cells/tree/main/tools/nodecode-cron) cell: schedule a prompt that calls `linear_digest`, and the list is delivered by itself.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The query, the paging, the four groups, the section, the `linear_digest` tool, the `/linear-digest` command and the manual. |
| `package.lisp` | The package, its nickname and the peripheral declaration. |
| `config.example.jsonc` | The section this cell reads, commented; documentation, not config. |
