# nodecode-agent-batch

Agent Batch: phases a task list into parallel groups and dispatches each group to agents on GitHub Actions. A Nodecode rewrite of [raaaas/hermes-agent-batch](https://github.com/raaaas/hermes-agent-batch) by [raaaas](https://github.com/raaaas).

Give it a task list, let the model group the tasks that can run at the same time, then dispatch a group: a workflow in your own repository starts one coding agent per task, each on its own branch, and opens a pull request. A later group starts after the earlier one lands, so the plan is the order.

## Get it

Tell the agent "add the agent batch cell", or run `nodecode add nc://agent-batch`.

## Set it up

The `agent-batch` section of `config.jsonc` names where the work runs:

| Member | What it is |
| --- | --- |
| `repo` | `owner/name` of the repository the workflow runs in. |
| `token_env` | The environment variable holding a GitHub token, with `actions:write` and `pull-requests:write`. `GITHUB_TOKEN` is read either way; `token_file` names a file instead. |
| `workflow` | The workflow file in that repository that takes the input (default `agent-batch.yml`). |
| `base_branch` | The branch a dispatch runs on, which the agents branch from (default `main`). |
| `model` | The provider/model handed to every agent; empty means the runner's own default. |
| `state_dir` | Where `plan.json` and `runs.json` live (default `<home>/agent-batch/`). |

The repository side is a workflow of your own with a `workflow_dispatch` trigger and a `tasks` input (the original's `agent-batch.yml` is one; it takes `tasks`, `context`, `model` and `base_branch` and spawns one job per task). This cell sends the dispatch and reads back the runs; what the agents do is that workflow's business.

## Use it

Ask the agent for what you want done, then "phase these and dispatch the first group". The tools it calls:

| Tool | What it does |
| --- | --- |
| `agent_batch_tasks` | Store the task list, one task per entry. Storing a list clears the old phasing. |
| `agent_batch_plan` | Read the tasks by index, the phases, their status, and what this machine dispatched. |
| `agent_batch_phase` | Save the groups, in the order they run: `[[0, 1], [2]]`, or the tasks' text. Every task in exactly one group, or the call is refused. |
| `agent_batch_dispatch` | Launch one group through `workflow_dispatch`. Real CI, real branches, your Actions minutes. |
| `agent_batch_runs` | Recent workflow runs and open pull requests of the repository. |

Type `/agent-batch` for the plan, `/agent-batch dispatch N` to launch a group, `/agent-batch reset` to forget it. `(help :agent-batch)` is the same workflow as prose, for the model.

The plan is the original's own JSON, so a `plan.json` made by the Hermes plugin reads here and the other way round.

## License

MIT. The plugin this is rewritten from is `raaaas/hermes-agent-batch` at `064afc5d`, MIT: see `LICENSE` and `NOTICE`.
