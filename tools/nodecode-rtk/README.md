# nodecode-rtk

RTK: rewrites each shell command through the `rtk` binary before it runs, so the model reads compact output. A Nodecode rewrite of [kerrz2020/hermes-rtk-rewrite](https://github.com/kerrz2020/hermes-rtk-rewrite) by [kerrz2020](https://github.com/kerrz2020).

`git status`, a test run, `ls`, `grep`: the raw output of a command is what a model pays for on
every round that follows. [RTK](https://github.com/rtk-ai/rtk) is the command proxy that
prints it compressed — and every command it does not touch runs exactly as it did. This cell
puts the original plugin's one move in front of the shell: the command is handed to
`rtk rewrite` before anything runs, and when rtk accepts a rewrite (exit 0 or 3) the call
runs the rewritten command, which is `rtk <command>`. It is the original's `pre_tool_call`
hook on the seam Nodecode has for the same moment: the `:TOOL` point, where an `eval`
snippet's `(sh "...")` forms and a shell tool's `command` argument can still be changed.

## Get it

Tell the agent "add the rtk cell", or run `nodecode add nc://rtk`.

## Set it up

Install RTK first — the cell is only a bridge, and without the binary it quietly does
nothing. Its releases are at <https://github.com/rtk-ai/rtk/releases>; the original plugin's
guide pins `0.49.0` and verifies the release checksum, and that is a fine way to install it.
`rtk --version` answering is the whole test; `/rtk check` asks the cell to probe it again.

The section `rtk` in the config tunes it; every member may be left out.

| Member | Default | What it does |
| --- | --- | --- |
| `binary` | `rtk` | the program, searched on PATH |
| `timeout_seconds` | 2 | one rewrite's ceiling; past it the command runs as written |
| `rewrite_eval` | true | rewrite `(sh "...")` calls inside an eval snippet |
| `extra_tools` | — | more tool names whose `command` argument is rewritten |

## Use it

It works while the model works. A command is rewritten only when rtk accepts the rewrite;
when it answers 1 or 2 (its expected passthrough), when it times out, when it is not
installed, or when the call is not a shell command, the call runs exactly as the model wrote
it. That is the original's rule, and its fail-open habit: no rewrite, never a blocked or a
broken command.

`/rtk` says what it has rewritten since the start — commands rewritten, commands left as
written, and the binary it runs with — `/rtk check` probes the binary again after an install,
and `/rtk reset` zeroes the counts.

Try `(sh "git status")` in a repository after installing rtk: the receipt shows
`rtk git status`, and the output is rtk's summary rather than git's.

What of the original it does not do: the desktop pane and status chip
(`desktop/plugin.js`), the binary updater and the maintainer's sync script (`scripts/`). The
rewrite engine is RTK's Rust binary either way; this cell starts no download, no update and
no process but the one `rtk` call the user's own install provides.

Apache-2.0. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
