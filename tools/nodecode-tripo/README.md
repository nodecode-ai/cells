# nodecode-tripo

Tripo 3D: text, a photo or a few views become a textured model the model can then rig, animate, retopologise and convert. A Nodecode rewrite of [Adolanium/hermes-tripo](https://github.com/Adolanium/hermes-tripo) by [Adolanium](https://github.com/Adolanium).

The model gets the original's fourteen tools, `tripo_generate_model`,
`tripo_generate_image`, `tripo_multiview`, `tripo_texture`, `tripo_parts`,
`tripo_uv`, `tripo_rig_check`, `tripo_rig`, `tripo_animate`,
`tripo_retopology`, `tripo_convert`, `tripo_task`, `tripo_download` and
`tripo_balance`; `/tripo` shows the credit balance; and `(help :tripo)` is
the page the model reads.

## Get it

Tell the agent "add the tripo cell", or run `nodecode add nc://tripo`.

## Set it up

A Tripo API key, from [platform.tripo3d.ai](https://platform.tripo3d.ai),
in the `tripo` section of `config.jsonc` (or `/settings`), or in the
environment as `TRIPO_API_KEY`:

```jsonc
{
  "tripo": {
    "api_key": "tsk_…",          // or api_key_env: "TRIPO_API_KEY"
    "base_url": "https://openapi.tripo3d.ai/v3",   // the default
    "download_folder": "",       // empty means <home>/tripo
    "timeout_seconds": 60
  }
}
```

A section with no key refuses at start, naming the member; the `tripo`
section is then one edit away in `/settings`.

| Member | Default | What it does |
| --- | --- | --- |
| `api_key` | unset | the Tripo key; `TRIPO_API_KEY` and `api_key_env` are read too |
| `api_key_env` | unset | the environment variable holding the key |
| `base_url` | `https://openapi.tripo3d.ai/v3` | the API root; https, or plain http on this machine |
| `download_folder` | unset | where `tripo_download` saves when a call names no `save_dir` |
| `timeout_seconds` | `60` | one request's ceiling |

## Use it

Everything starts with a task. Say what a call will cost first: every
creating tool states its estimate in its answer, and the same estimate is
computed from the request that gets sent.

```
tripo_generate_model {prompt: "a brass treasure chest, worn edges"}   -> task_…
tripo_task {task_id: "task_…"}                                        -> status, progress, outputs
tripo_download {task_id: "task_…", save_dir: "assets/"}               -> the files on disk
```

A text prompt, an image or multiview views go in; a rigged, animated,
retopologised or converted model comes out through `tripo_rig`,
`tripo_animate`, `tripo_retopology` and `tripo_convert`, each of which
takes the task id of the step before. `tripo_task` polls for at most
`wait_seconds` (120 by default, 300 at most): a timeout leaves the task
running, so call it again with the same id instead of making a second one.
Output URLs expire, so `tripo_download` soon after a task succeeds; it
writes next to the name it is given and never overwrites a file it finds.

An input is a task id, a file_token, a public https URL, or a local file
path, which is uploaded to Tripo first (image and model types, at most
50 MB).

Run `.github/test-cell.sh tools/nodecode-tripo` from the cells repository to
run the tests: they stub every HTTP call, so nothing reaches Tripo.

## What the rewrite leaves out

The original also had a Hermes Desktop half (an inline three.js viewer, a
library pane, a status-bar chip), an approval prompt before each paid call,
a per-chat spend counter and a local task history. A Nodecode cell has no
page of its own to put a viewer on, and a tool call arrives with no
approval seam, so the answer states the cost and the model says it before
it spends; the task ids the answers return are the history. See `NOTICE`
for the full account.
