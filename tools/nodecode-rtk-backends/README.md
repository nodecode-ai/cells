# nodecode-rtk-backends

RTK Backends: rewrites each shell command through the `rtk` binary before it runs, so the model reads compact output, for the backends a profile opts in to. A Nodecode rewrite of [petertaras/hermes-rtk-rewrite-with-docker-support](https://github.com/petertaras/hermes-rtk-rewrite-with-docker-support) by [petertaras](https://github.com/petertaras). That plugin is itself a fork of [kerrz2020/hermes-rtk-rewrite](https://github.com/kerrz2020/hermes-rtk-rewrite), which derives from the [RTK Contributors'](https://github.com/rtk-ai/rtk) own Hermes hook. Apache-2.0; see [LICENSE](LICENSE) and [NOTICE](NOTICE).

`git status`, a test run, `ls`, `grep`: the raw output of a command is what a model pays for on
every round that follows. [RTK](https://github.com/rtk-ai/rtk) is the command proxy that
prints it compressed — and every command it does not touch runs exactly as it did. This cell
puts the original plugin's one move in front of the shell: the command is handed to
`rtk rewrite` before anything runs, and when rtk accepts a rewrite (exit 0 or 3) the call
runs the rewritten command, which is `rtk <command>`. It is the original's `pre_tool_call`
hook on the seam Nodecode has for the same moment: the `:TOOL` point, where an `eval`
snippet's `(sh "...")` forms and a shell tool's `command` argument can still be changed.

## Get it

Its sibling `nc://rtk` rewrites every command; this one rewrites only those whose backend
you opt in to, which is the fork's addition. Tell the agent "add the rtk-backends cell", or
run `nodecode add nc://rtk-backends`.

## Set it up

Install RTK first — the cell is only a bridge, and without the binary it quietly does
nothing. Its releases are at <https://github.com/rtk-ai/rtk/releases>; the original plugin's
guide pins `0.49.0` and verifies the release checksum, and that is a fine way to install it.
`rtk --version` answering is the whole test; `/rtk-backends check` asks the cell to probe it
again.

The section `rtk-backends` in the config tunes it; every member may be left out.

| Member | Default | What it does |
| --- | --- | --- |
| `binary` | `rtk` | the program, searched on PATH |
| `timeout_seconds` | 2 | one rewrite's ceiling; past it the command runs as written |
| `rewrite_eval` | true | rewrite `(sh "...")` calls inside an eval snippet |
| `extra_tools` | — | more tool names whose `command` argument is rewritten |
| `backends` | `["local"]` | backends whose commands may be rewritten; `all` names every one |

## The backend gate

A rewritten command needs `rtk` on the machine that **runs** it, and the original reads that
machine off Hermes's terminal policy. Nodecode has one shell, so the cell reads it off the
command itself: its first program, after the prefixes that only pass a command along
(`sudo`, `env`, a `VAR=value` assignment). A command whose program hands the work to a
container or another host — `docker`, `podman`, `kubectl`, `ssh`, `lxc`, `daytona` and their
kind — is rewritten only when `backends` names that backend; every other command runs in
`local`. That is the original's rule: the profile's default is local only, a container or
remote backend opts in once its image carries RTK, and a backend the cell cannot tell is
passed through unasked.

## Use it

It works while the model works. A command is rewritten only when its backend is opted in and
rtk accepts the rewrite; when it answers 1 or 2 (its expected passthrough), when it times
out, when it is not installed, or when the call is not a shell command, the call runs
exactly as the model wrote it. That is the original's rule, and its fail-open habit: no
rewrite, never a blocked or a broken command.

`/rtk-backends` says what it has rewritten since the start — commands rewritten, left as
written, passed over for a backend nobody opted in, and the backends it runs for — and names
the binary it runs with. `/rtk-backends docker` says whether one backend is opted in,
`/rtk-backends check` probes the binary again after an install, and `/rtk-backends reset`
zeroes the counts.

Try `(sh "git status")` in a repository after installing rtk: the receipt shows
`rtk git status`, and the output is rtk's summary rather than git's. Try
`(sh "docker ps")` with the default `backends`: the receipt shows `docker ps`, untouched.

What of the original it does not do: the desktop pane and status chip
(`desktop/plugin.js`), the binary updater and the maintainer's sync script (`scripts/`), and
the optional `container/Containerfile`, which builds an image carrying RTK for a Docker
backend of the user's own — this cell starts no download, no update and no process but the
one `rtk` call the user's own install provides, so a Docker backend is opted in only after
`rtk` is installed in it.

Apache-2.0. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
