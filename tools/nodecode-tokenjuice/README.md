# nodecode-tokenjuice

Tokenjuice: compact successful terminal output before the model reads it. A Nodecode rewrite of [vincentkoc/tokenjuice](https://github.com/vincentkoc/tokenjuice) by [vincentkoc](https://github.com/vincentkoc).

A build log, a test run, a `find`: most of what a shell answers is noise the model pays
for on every round that follows. This cell folds that output on its way into the
context — the first lines, the last lines, and a count of what went between them —
without touching the command that ran. It is the original plugin's one hook
(`transform_terminal_output`) on the seam Nodecode has for the same moment: the
`eval`/`sh` tool result, rewritten after the call and before the model reads it.

## Get it

Tell the agent "add the tokenjuice cell", or run `nodecode add nc://tokenjuice`.

## Set it up

Nothing is required. The section `tokenjuice` in the config tunes it; every
member may be left out.

| Member | Default | What it does |
| --- | --- | --- |
| `min_lines` | 12 | the fewest lines worth compacting |
| `head_lines` | 10 | lines kept from the start |
| `tail_lines` | 10 | lines kept from the end |
| `max_line_chars` | 240 | the longest one line may be kept, folded in its middle |
| `min_saved_chars` | 200 | write nothing when the fold saves less than this |
| `dedupe` | true | drop a line that repeats the one right before it |
| `strip_ansi` | true | drop colour and cursor codes |
| `keep_failures` | true | leave a failing command's output whole |
| `extra_tools` | — | more tool names whose results are terminal output |

## Use it

It works while the model works. A result is rewritten only when it is long
enough, when the command succeeded (no `[exit N]` other than 0 and no timeout),
and when folding saves at least `min_saved_chars` characters; anything else comes
back exactly as it was, which is the original's rule too.

`/tokenjuice` says what it has saved since the start — results compacted,
characters and lines kept out of the context, and the counts it runs with —
and `/tokenjuice reset` zeroes that.

What of the original it does not do: the original defers to the `tokenjuice` CLI,
which carries about ninety per-command rules (`git status`, `git diff`, `rg`,
install logs and more) and a local configuration file. This rewrite reads the tool
call itself and folds every command with one rule, so it needs no Node.js and no
second install; see NOTICE.

MIT-licensed. See [LICENSE](LICENSE).
