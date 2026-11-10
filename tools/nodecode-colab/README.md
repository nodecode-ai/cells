# nodecode-colab

Colab GPU Runner: run a Python file on a Google Colab GPU runtime and bring its outputs home. A Nodecode rewrite of [PietrangeloManco/hermes-colab](https://github.com/PietrangeloManco/hermes-colab) by [PietrangeloManco](https://github.com/PietrangeloManco).

## Get it

Tell the agent "add the colab cell", or run `nodecode add nc://colab`.

## Set it up

The cell uses your own Google account, the way the Colab web page does, and never sees your password: it needs an OAuth grant with the Colaboratory scope.

1. In the [Google Cloud console](https://console.cloud.google.com/), create or pick a project, set the OAuth consent screen to **External** and add your Google account under **Test users**.
2. *Credentials → Create credentials → OAuth client ID*, application type **Desktop app**. Keep its client id and secret.
3. Sign in once with the Google account you use on Colab and keep the grant in the `colab` section: either `access_token` (an hour), or `refresh_token` with `client_id` and `client_secret`, which is what lets the cell renew it by itself.

Then ask for `colab_doctor` — every check names the exact next step — and `/colab` lists the runtimes assigned to the account.

## Use it

| Tool | What for |
| --- | --- |
| `colab_run` | Run a `.py` on `T4`, `L4`, `A100`, `H100` or a CPU runtime. `accelerator`, `requirements`, `env`, `name`, `keep_runtime`, `high_mem`, `timeout_minutes`. Answers at once with a run id. |
| `colab_status` | One run: state, accelerator, elapsed time, outputs, events. `all` lists the recent ones. |
| `colab_logs` | The tail of the run's stdout and stderr. |
| `colab_cancel` | Interrupt the kernel, sync, release the runtime. |
| `colab_runtimes` | The runtimes assigned to the account. |
| `colab_release_runtime` | Free one by endpoint. |
| `colab_doctor` | The Google grant, Colab access, the active runs. |

A run writes its file to `/content/nodecode_colab/<run>/` on the VM, executes it there with that folder as the working directory, and brings anything the job writes under `outputs/` back to `~/.nodecode/colab_runs/<run>/outputs/` while it runs; the log lands beside it in `run.log`. A script that reads its last checkpoint from `$COLAB_OUTPUT_DIR` and honours `COLAB_RESUME=1` continues on a fresh runtime when Colab reclaims the VM, up to `max_resumes` times. Runtimes consume the account's compute units like the web page does and are released when the run ends unless `keep_runtime` says otherwise; check your plan's current rates in Colab.

The operator can type `/colab` for the connection and the runs, `/colab logs` and `/colab cancel`, and `/colab <run>` for one run.
