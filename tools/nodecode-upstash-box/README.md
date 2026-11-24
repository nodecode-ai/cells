# nodecode-upstash-box

Upstash Box: a cloud Linux container to run commands and keep files in. A Nodecode rewrite of [upstash/box](https://github.com/upstash/box) by [upstash](https://github.com/upstash).

The original moves a whole agent's terminal into a Box. Nodecode's shell is
its own, so what crosses over here is the Box itself: four tools that create,
wake and use one over Upstash's API, and `/box` to see, pause or delete the
one this session holds.

| Tool | What it does |
| --- | --- |
| `box_exec` | Runs one shell command in the Box, `sh -c`, and answers its exit code and output. |
| `box_read` | Reads a text file from the Box. |
| `box_write` | Writes a text file in the Box, whole. |
| `box_list` | The Boxes this account may use, with id, name and state. |

Ask "install ripgrep in the box and grep this project there", "build it in a
clean Linux box", or "run this script where it cannot touch my machine" — the
model calls `box_exec` and brings back what the command answered.

The first command of a session creates the Box, which takes a few seconds;
every later one runs in the same Box. When the cell stops, the Box is
**paused, not deleted**: its files and installed tools are still there next
time, and the next command wakes it. `/box delete` is how a Box goes away.

## Get it

Tell the agent "add the upstash-box cell", or run `nodecode add nc://upstash-box`.

## Set it up

Two things: an Upstash account with a Box API key (the console's **Box**
section), and the key where the cell can read it — `UPSTASH_BOX_API_KEY` in
the environment, or the `upstash-box` section of `config.jsonc` (also
editable in `/settings`):

```jsonc
{
  "upstash-box": {
    "api_key_env": "UPSTASH_BOX_API_KEY",
    "size": "small",        // small | medium | large
    "runtime": "python",    // the Box base image: node, python, golang, ruby, rust, ...
    "keep_alive": false     // true: never paused, and billed while it runs
  }
}
```

`base_url` moves the cell to another region's endpoint
(`https://us-east-1.box.upstash.com` is the default). Boxes come in fixed
sizes, so `size` is the only way to give one more CPU or memory, and the
default image (`node`) has Python 3 but no `pip`: set `"runtime": "python"`
for Python work.

## Use it

`box_exec` is the one to reach for. A command starts in the Box user's home;
pass `cwd` from a second command on, because each call is its own shell. The
output the model is shown is capped by `max_output_chars`, and
`timeout_seconds` bounds one request — a command that runs past it is cut
off on this side, not in the Box.

Nothing crosses over by itself. A file on this machine is not in the Box
until `box_write` puts it there, and a file the Box writes stays there until
`box_read` or another command brings it back.

`/box` shows the Box this session holds and what state it is in; `/box list`
the account's Boxes (its own label included); `/box pause` stops the one it
holds; `/box delete` destroys it and its files.

### What leaves your machine

Every command, and every file `box_write` sends, goes to Upstash's API and
runs in your account's Box — into which the API injects the account's own
Box environment (including an `UPSTASH_BLOB_TOKEN`, and `~/.git-credentials`
when the account has GitHub connected, which a command can read). The API key
travels in the `X-Box-Api-Key` header and is never put in the Box. The cell
sets no telemetry of its own: the API's own metrics aside, it sends only
what the commands and files are.

### Differences from the original

- The original syncs your skills, cache and skill-registered credentials into
  every Box and copies changed ones back. This cell syncs nothing: what is in
  the Box is what a command or `box_write` put there.
- The original keeps one Box per task, named after the task, and can start a
  fresh one per session instead. This cell holds one, named for itself, under
  the home; `/box delete` is the per-session reset.
- The original runs an interactive exec session per command, streaming output
  and killing a timed-out process tree inside the Box. This cell runs one
  request per command through `POST /exec`, so a command outliving its
  timeout is cut off here and may keep running in the Box.
- Approval of dangerous commands is Nodecode's own; the Box is where a
  command runs, not where it is approved.

## License

MIT. The original is MIT, Copyright (c) 2026 Upstash, Inc.; see `NOTICE` and
`LICENSE`.
