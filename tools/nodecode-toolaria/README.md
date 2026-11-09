# nodecode-toolaria

Toolaria: rescues oversized tool results before they flood the context, by spilling them to disk and handing the model a short preview plus a fetch handle. A Nodecode rewrite of [Sahil-SS9/Toolaria](https://github.com/Sahil-SS9/Toolaria) by [Sahil-SS9](https://github.com/Sahil-SS9).

## Get it

Tell the agent "add the toolaria cell", or run `nodecode add nc://toolaria`.

## Set it up

Nothing is required. The section is `toolaria` in `~/.nodecode/config.jsonc`, and every field has a working default:

```jsonc
{
  "toolaria": {
    "max_result_chars": 12000,     // a result longer than this is rescued
    "head_lines": 40,              // lines the preview keeps from the start
    "tail_lines": 15,              // and from the end
    "excerpt_max_chars": 8000,     // the hard cap on the preview
    "fetch_max_chars": 4000,       // the cap on one range or grep answer
    "full_fetch_max_chars": 50000, // full mode refuses a blob over this
    "store_path": "~/.nodecode/toolaria",
    "ttl_hours": 72,               // older blobs are swept at start
    "keep_out": ["rescuer_fetch", "eval"]
  }
}
```

`keep_out` names tools whose results are never spilled. `eval` is there by default because its output is usually the thing the operator asked to see, and the fetch tool's own answers must not be re-spilled.

## Use it

Nothing to call: the cell hooks the core's `:TOOL` point, so every tool result longer than `max_result_chars` comes back as a preview plus one handle line:

```
[Toolaria: tool result rescued. tool=web_extract; at=2026-11-08 04:12:31; size=171,874 chars; lines=1,994; blob=5ddff67d3a1b]
Preview (first 40 / last 15 lines); this is a preview, NOT the full output:
...
Retrieve more with rescuer_fetch(id="5ddff67d3a1b", mode=...): outline | search(query) | grep(pattern) | range(start,count) | stat | full
```

The model then reads only the slices it needs, with `rescuer_fetch`:

| mode | what it answers |
| --- | --- |
| `outline` | the blob's blocks and their first lines, with line numbers |
| `search` | the lines ranked by the words they share with `query` |
| `grep` | the lines matching the regex `pattern`, numbered, with a count |
| `range` | `count` lines from `start`, counting from 0 |
| `stat` | size, line count, the tool it came from, and the modes |
| `full` | everything, under `full_fetch_max_chars` |

`/rescuer` answers the store's line: how many blobs, how much disk, where they are, the thresholds, and what is kept out. `(help :toolaria)` carries the same in the model's manual.

Blobs are content-addressed - the first 12 hex characters of the text's SHA-256 - so the same result rescued twice is one blob, and a handle stays valid until the blob is swept after `ttl_hours`.

## What the rewrite leaves out

Pass-by-reference (`tla:<id>` expanded inside another tool's arguments), which needs a host middleware this contract does not expose; embedding search, so `search` ranks lexically and the cell needs no Python or model download; the optional at-rest encryption and its key rotation; hot-blob pinning and the size-budget sweep; the chained `<id>@N` grammar and the expansion audit ledger. `NOTICE` says the same, in the original's terms.

## Test

```sh
.github/test-cell.sh tools/nodecode-toolaria
```

Offline: every test starts the cell against a store in the temporary directory, pushes a result through the installed hook, and reads it back through every fetch mode.
