# nodecode-file-lineage

File-lineage: a receipt for every file the agent writes, naming the files, pages and
searches it was made from. A Nodecode rewrite of [P0GL0L/hermes-lineage](https://github.com/P0GL0L/hermes-lineage) by [P0GL0L](https://github.com/P0GL0L).

An agent writes a report and nobody can say which notes, which page or which
earlier file it came from. This cell watches every tool call, and when a call
writes or changes a file it records the file's SHA-256, the call that made it,
and the sources around it — each labelled by how it is known, never blended
into one vague "derived from":

- **observed** — the producing call named the source: the path in its
  arguments, the URL in its command.
- **evidenced** — the artifact's own text names it, and this session had
  already seen it.
- **inferred** — this session read it earlier in the turn window.

A file a shell or `eval` call changed is found by walking the session's folder
before and after the call, so `make`, a generator or a script is attributed
too. Credential files (`~/.ssh`, `~/.aws`, `.env`, `*.pem`, `id_rsa`…) are
never opened and never recorded.

## Get it

Tell the agent "add the file-lineage cell", or run `nodecode add nc://file-lineage`.

## Set it up

Nothing is required. The `file-lineage` section of `~/.nodecode/config.jsonc`
(or `/settings`) bounds what one call may cost:

| Member | Values | Default |
| --- | --- | --- |
| `sidecar` | write `<file>.receipt.json` beside every artifact | `false` |
| `track_shell` | attribute the files a `sh`, `eval` or `patch` call changed | `true` |
| `snapshot_max_files` | files the folder walk reads after a shell call | `2000` |
| `max_artifacts_per_call` | files one call may record | `50` |
| `session_window_calls` | earlier sources counted as inferred; `0` disables | `40` |

## Use it

The cell works on its own: every tool call that writes a file leaves a
receipt. Then:

```
/file-lineage                 artifacts made in this session
/file-lineage <file>          where a file came from
/file-lineage receipt <file>  write <file>.receipt.json beside it
/file-lineage verify <file>   check the file and its sources against recorded hashes
/file-lineage mermaid <file>  the graph back to the sources
/file-lineage stats           what the store holds
```

`verify` re-hashes the file and each file source and says `fresh`,
`stale`, `modified` or `missing`; URLs are `unchecked`, because nothing here
reaches the network. The model has the same two answers as tools,
`file_lineage_show` and `file_lineage_verify`, and `(help :file-lineage)` says so.

The receipt is JSON and stands on its own: it carries the artifact's hash,
size and version history, the producer, and every source with its basis and
its own hash, plus a digest of the receipt body (`receipt_sha256`), so a
copied pair of file and receipt can be checked without the database.

What is stored: paths, URLs, hashes, sizes, times, the tool name and the call
id, under the cell's home (`file-lineage/graph.json`). No prompt, no file body and
no credential is kept.
