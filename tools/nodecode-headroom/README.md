# nodecode-headroom

Headroom: keeps a small machine alive while the agent builds, installs and tests. A Nodecode rewrite of [seongj-un/hermes-headroom](https://github.com/seongj-un/hermes-headroom) by [seongj-un](https://github.com/seongj-un).

Heavy shell commands — installs, builds, test suites, browsers, compilers — are the ones
that end a small box. Headroom puts four things between them and the out-of-memory killer:

- a **slot queue**, so only so many heavy jobs run at once, however many sessions are open;
- a **memory gate**, so a job starts only when the host has room for it beyond a reserve;
- a **lighter command**: a Node heap cap and single-worker flags, put into the form before
  it runs, so what runs is what the form says;
- a **watchdog** that stops a process of the job when available memory crosses the critical
  line, so the image and its session survive instead of the job.

The model is told each of them in the command's own result: it waited, it ran as this, a
process of it was stopped. Nothing is silent.

## Get it

Tell the agent "add the headroom cell", or run `nodecode add nc://headroom`.

## Use it

Nothing to do: it is on from the first launch of a folder that holds it, and a light
command — `git status`, `rg`, a `ls` — is not touched. What it will do is visible before
you run something:

```
/headroom                     free memory, the next job's cap, the jobs running now
/headroom events              recent queue waits, caps and watchdog stops
/headroom explain npm run build
```

and the model has the same picture as JSON from the `headroom_status` tool, which its
description tells it to call before heavy work on a small machine. `(help :headroom)` is
the same manual from eval.

The `headroom` section tunes it, all with defaults that fit a machine nobody described:

| key | default | what it does |
| --- | --- | --- |
| `mode` | `enforce` | `observe` only logs what it would do; `off` does nothing |
| `max_heavy_jobs` | `0` | heavy jobs at once; `0` = one below 4 GB of RAM, two below 16 GB, else four |
| `reserve_mb` | `0` | kept for the image and the system; `0` = 15% of RAM, 256 MB to 2 GB |
| `min_start_mb` | `0` | free memory a job needs beyond the reserve; `0` = 10% of RAM, 192 MB to 1 GB |
| `critical_mb` | `0` | the watchdog's line; `0` = 5% of RAM, 96 MB to 512 MB |
| `queue_timeout_s` | `300` | how long a command waits for a slot and memory before it is refused |
| `lighten` | `true` | the flags and the heap cap |
| `watch` | `true` | the watchdog |
| `extra_heavy` | `[]` | regular expressions for work the built-in table does not know |
| `exclude` | `[]` | regular expressions over the whole command: never heavy |

A refused command is a failed tool result, never a dead turn: the model reads why, and
what to try instead.

## What it reads and what it changes

It reads `/proc`: `meminfo`, `pressure/memory`, `cpuinfo` and the `stat` and `status`
lines of running processes. Nothing leaves the machine, no key is used, no network call is
made. It changes one thing in the form before it runs — the command strings of `(sh ...)`,
`(uiop:run-program ...)` and the `($ "name")` strings object — and only to add flags and
an environment prefix. It never edits a file of yours, never installs anything, and never
touches a process it did not see start: a watchdog kills only a descendant of this image
that appeared during the watched call.

It is a guard, not a boundary: a job the agent starts outside `eval` (a background shell,
a sub-agent of its own) is not in the slot queue.

## Gaps

The original is a Hermes plugin for systemd hosts. Three of its parts have no counterpart
here, and are named rather than faked:

- **the isolated, memory-capped scope.** Hermes moves a job into a `systemd --user` scope
  with `MemoryMax` and a raised OOM score, and reads the cgroup's `memory.peak` and its
  OOM kill count. Nodecode has no seam that hands one child to systemd, so this rewrite
  caps what it can — the V8 heap, and the parallelism — and reports the budget a scope
  would have had. A `:TOOL` hook cannot see the process handle the shell returns either,
  so the watchdog works from `/proc`, not from a cgroup.
- **the machine-wide pool.** The original's slots are files with `flock`, shared by every
  Hermes process and profile on the host. This one is one semaphore in one image, which is
  the unit Nodecode runs; two Nodecode processes on one host each keep their own slots.
- **`/headroom selftest`.** It proves isolation by OOM-killing a test process inside a tiny
  scope. With no scope, there is nothing to prove; `headroom explain` and the tool's JSON
  cover what is left.

Everything else is carried: the classifier's families and its "a command word, never prose
in quotes" reading, the auto budgets, the lighten table (`--runInBand`, `--workers=1`,
pytest's `-n 0`-able xdist, `make -j`, `cargo -j`, `go -p`, `gradle --max-workers`), the
per-job Node heap cap, the queue timeout that refuses with an explanation, and the model
being told what happened.

## Test it

```
.github/test-cell.sh tools/nodecode-headroom
```

The tests fabricate a `/proc` of their own — memory, pressure, a CPU count, two
processes — so they are offline and spawn nothing; the hook's next is a stub.
