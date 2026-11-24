# nodecode-tameru

Tameru (貯める, to save up): big tool results, kept small, nothing lost — the lines that matter and every distinct error, with the exact original one tool call away. A Nodecode rewrite of [0xWhiteMage/hermes-tameru-plugin](https://github.com/0xWhiteMage/hermes-tameru-plugin) by [0xWhiteMage](https://github.com/0xWhiteMage).

A test run, a build log, a directory listing: most of a long tool result is
noise, and it rides every later request at the model's expense. This cell
shortens such a result on its way into the context — the first lines, the
last lines, and the first line of every distinct error template, with one
marker line where a run of the middle went — and keeps the exact original, so
nothing is lost behind the shortening: `tameru_expand` hands any of it back,
whole, by line range, or by a regex.

| Surface | What it does |
| --- | --- |
| the hook | A tool result of `min_chars` characters or more comes back shortened, its first line ending in a `[tameru: kept … · ref=…]` header. A result that repeats one template forty times is one exemplar and its count (`[×40] …`). A result holding a probable credential is shortened but not kept. |
| `tameru_expand` | Reads the original back by the ref its header names: all of it, only `start_line`..`end_line`, or only the lines matching `grep`, up to `max_chars`. It says when a ref is unknown — the store is memory only, so a restart forgets it. |
| `/tameru` | What shortening has saved and which originals are held; `/tameru expand REF` prints one here; `/tameru reset` forgets both. |

Everything is local and deterministic — no model call, no network, no child
process, nothing written to disk.

## Get it

Tell the agent "add the tameru cell", or run `nodecode add nc://tameru`.

## Set it up

Nothing is required. The section `tameru` in the config tunes it; every
member may be left out.

| Member | Default | What it does |
| --- | --- | --- |
| `min_chars` | 4000 | the smallest result worth shortening |
| `head_lines` | 8 | lines kept from the start |
| `tail_lines` | 8 | lines kept from the end |
| `max_line_chars` | 300 | the longest one kept line may be, folded in its middle |
| `error_lines` | 40 | how many distinct error lines are kept |
| `store_limit` | 16 | how many originals `tameru_expand` can read back |
| `skip_tools` | — | more tool names whose results are never shortened |

## Use it

It works while the model works: a big result arrives short, and the model
may call `tameru_expand` when it needs a line the shortening left out. A
result is only rewritten when the reduction saves at least a fifth of its
characters; a short one, a result of `tameru_expand`'s own, and one from a
tool on the keep list come back exactly as they were.

What of the original it does not do: the original is a context engine that
subclasses another agent's compressor and prunes results already in the
history, on a token budget scaled to the model's window, with an archive
lookup after a restart; Nodecode has no compressor to subclass, so this cell
acts at the tool-result seam instead, keeps the original in memory rather
than in a session archive, and has no summariser pass. See NOTICE.

MIT-licensed. See [LICENSE](LICENSE).
