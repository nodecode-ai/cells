# nodecode-duet-queue

Duet Queue: a background task queue with human approval gates and a worker that runs a task through your own agent CLI. A Nodecode rewrite of [benfirad/hermes-plugin-antigravity-duet](https://github.com/benfirad/hermes-plugin-antigravity-duet) by [benfirad](https://github.com/benfirad).

A long task should not hold a conversation open, and a model should not be able to start one on your machine without your word. The queue is both: the model adds a task, it waits in `awaiting_approval`, you run `/duet approve 3`, and a worker runs it in the background through the CLI you named, keeping its output on the task for later. A task that ends by asking for one more decision goes back to waiting instead of guessing.

## Get it

Tell the agent "add the duet-queue cell", or run `nodecode add nc://duet-queue`.

## Set it up

The cell needs the CLI that runs a task, and nothing else. In `~/.nodecode/config.jsonc`:

```jsonc
{
  "duet-queue": {
    "command": "agy",                              // your agent CLI, required
    "args": ["--print", "--no-tools"],            // flags, before the prompt
    "approval_required": true,                     // a model's task waits for you
    "auto_run": true,                              // drain the queue in the background
    "interval_seconds": 15,
    "timeout_seconds": 600,
    "max_tasks": 100
  }
}
```

`command` is the only required member: the cell refuses to start without it, and says so. `dir` moves the queue file and the task workspaces off the Nodecode home; `swarm_command` names the Ruflo CLI `duet_swarm` reads, when it is installed. No key, no account: it runs whatever CLI you already have logged in.

## Use it

In a shell:

```
/duet                               the queue, newest first, and what waits for you
/duet add reindex the archive       add a task of your own, runnable at once
/duet list                          the queue
/duet show 3                        one task: its result, its log, what it waits for
/duet approve 3                     let a task the model added run
/duet reject 3                      drop a task
/duet retry 3                       put a failed or rejected task back
/duet run [3]                       run that task now, or every runnable one
/duffy                              the same command, as the original spelled it
```

From the model, two tools:

- `duet_queue` with `action` `add`, `list` or `show` (and `title`, `category`, `task_id`). A task it adds waits for you unless `approval_required` is off; `approve`, `reject` and `retry` are yours, and the model is told so when it tries.
- `duet_swarm` with `action` `status` or `agents`, read off the Ruflo CLI.

A finished task's output is on the task, and the notice at the top of the shell says whether it ran, failed, or is waiting for you.

## What it keeps, and where

Everything the cell knows is one JSON file, `queue.json`, under `duet-queue/` in the Nodecode home (or the `dir` you set), written through a temporary file and a rename, so a reader sees the old queue or the new one. Each task has a workspace of its own beside it, `work/task-<id>/`, and the CLI runs there. Ids are kept with the queue and never reused.

## The original

This is a rewrite, not a wrapper: no Node, no Python, no part of the original is bundled. [NOTICE](NOTICE) says what was kept and what was left out; [LICENSE](LICENSE) is the original's MIT licence, unchanged.
