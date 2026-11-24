# nodecode-receipts

Receipts: timestamped evidence of explicit file, test and website checks. A Nodecode rewrite of [iket72183-collab/hermes-receipts](https://github.com/iket72183-collab/hermes-receipts) by [iket72183-collab](https://github.com/iket72183-collab).

Your agent says it finished. See what actually passed.

## Get it

Tell the agent "add the receipts cell", or run `nodecode add nc://receipts`.

## Set it up

Nothing is required. The section is optional:

| Setting | What it is |
| --- | --- |
| `directory` | where the receipts are kept; empty means the `receipts/` folder of the nodecode home |
| `read_cap_bytes` | the most bytes one file read or page response covers (default 1048576) |
| `command_timeout` | the longest a test check may run, in seconds (default 60) |

## Use it

Ask the agent to declare, before the work, what would count as done, then to check it after. It has four tools:

- `receipts_begin` takes a spec: `schema_version` 1, `task_id`, an absolute `workspace_root`, and 1 to 20 checks. A **file** check names a workspace-relative path and either the text it must contain or the keys its JSON object must carry. A **test** check names a command, an absolute workdir, the exit code it must end with, optional output it must contain and a timeout. A **website** check names a url, the status it must answer and text it must hold. It answers an attempt id and a criteria digest, and runs nothing.
- `receipts_check` reads the file and page checks now, runs the test commands here (bounded, in their workdir), and seals one receipt.
- `receipts_get` reads back every receipt kept for a task id.
- `receipts_export` writes the sealed receipt as JSON or as a standalone HTML page under the receipts folder, and answers its filename. Public, the default, drops absolute paths, command lines and raw output.

`/receipts TASK-ID [ATTEMPT-ID]` lists a task's receipts. `(help :receipts)` has the same in the model's own words.

A receipt is **verified** when every check passed, **failed** when one requirement did not hold, and **unverified** when evidence is missing, timed out or could not be read. A sealed attempt is never rewritten: fix the work and begin a new attempt, and the earlier failure stays on the record. It is an as-of snapshot of the checks that were named -- not a sandbox, a signature or a certificate that the whole task is right.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The manual, the four tool contracts, the verbs, `/receipts`, and the cell's declaration. |
| `spec.lisp` | The bounded spec: validation, the canonical form, the criteria digest. |
| `checks.lisp` | The three adapters: file read with size and SHA-256, command run, page GET. |
| `store.lisp` | One JSON file per attempt under the receipts folder; sealing; exports. |
| `export.lisp` | The public view, the JSON document and the HTML page. |
| `json.lisp` | One small JSON writer, and the readers over decoded tables. |

## Differences from the original

The original observes the harness's own terminal calls through hooks. A Nodecode cell has no such per-command hook, so a test check runs the command it declares, bounded, in its declared workdir; the receipt says so. The `hermes receipts` CLI and the freshness window are not carried over.

## License

MIT. See [LICENSE](LICENSE); the original's copyright line is kept there, and [NOTICE](NOTICE) says what was rewritten.
