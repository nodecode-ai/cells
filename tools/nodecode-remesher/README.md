# nodecode-remesher

Remesher: one image into a GLB through the Remesher CLI you already run, with a job receipt that outlives the call and hashed artifacts. A Nodecode rewrite of [remesher/remesher](https://github.com/remesher/remesher) by [remesher](https://github.com/remesher).

The tools are the original Hermes plugin's, with the same names and arguments:

| Tool | What it does |
| --- | --- |
| `remesher_generate` | Submits one bounded image-to-GLB job and answers its `job_id` at once. |
| `remesher_job` | The persisted state of a job: queued, running, succeeded, failed, timed_out, interrupted. |
| `remesher_assets` | A successful job's GLB files, each with its size and SHA-256. |
| `remesher_health` | The backend's connectivity through the CLI's own `health`, or a native-local configuration. |

Ask "turn this photo into a 3D model", and the model submits the job, polls it, and hands you the GLB path and hash.

## Get it

Tell the agent "add the remesher cell", or run `nodecode add nc://remesher`.

## Set it up

The cell installs nothing: it drives a Remesher CLI you already have (the plugin's `comfy-prompt-cli`), on Linux, with its own Python environment. Native backends may take the GPU; the cell never provisions one.

```jsonc
"remesher": {
  "cli": "/opt/remesher/.venv/bin/comfy-prompt-cli",
  "backend": "comfy3d",
  "config": "/opt/remesher/config.json",
  "workflow": "/opt/remesher/workflows/comfy3d.json"
}
```

Three backends, the original's:

| `backend` | What it needs |
| --- | --- |
| `comfy3d` (default) | `cli`, plus `config` (the CLI's `config.json`, naming your ComfyUI server) and `workflow` (the Comfy3D workflow JSON). |
| `native` | `cli`, and optionally `native_server` — a **numeric loopback** origin such as `http://127.0.0.1:8188` — and `native_model` (`trellis2` or `pixal3d`). |
| `native-local` | `cli`, `native_python` (the checkout's own interpreter, symlinks unresolved so the venv keeps its identity), `native_checkout` (the directory holding `main.py`), and optionally `native_device` (`cuda` or `cpu`). |

`cli` is required: without it, or with a path that is not there, the cell refuses at start and says which member is missing. For a native-local setup, run the CLI's own `native-setup plan` and `check` first — this cell only inspects paths; the CLI owns readiness.

## Use it

You do not call the tools; the model does, when a request is about turning an image into a 3D model. A person can call the same functions from eval, in the `remesher` package:

```lisp
(remesher:job-state "0123456789abcdef0123456789abcdef")
```

`(help :remesher)` is the model's manual for the four verbs, kept while the cell runs.

### What a job is

- **One at a time.** A second submission while one is queued or running answers `busy` with the active id, as the original did: two jobs never race one GPU.
- **Bounded.** `timeout_seconds` is 1 to 1800 (default 900). At the bound the CLI's whole process group is killed, and the receipt says `timed_out`; the remote ComfyUI run is not cancelled, so check its queue before retrying.
- **Durable.** Each job is a directory under `remesher/jobs/<job_id>/` in the profile's home: the input image copied in, `output/`, `cli.log` (the CLI's own output, capped at 1 MiB) and `state.json`, written atomically. Nothing is replayed: a job runs once.
- **Checked.** An artifact is a file whose GLB header says `glTF`, version 2, at a length that matches the file. The manifest carries its size and SHA-256, and every read rehashes it: a receipt whose file is gone, changed or unreadable answers `integrity_error` with no assets rather than a stale path.
- **Scrubbed.** The CLI inherits `PATH`, `HOME`, `LANG`, `LC_ALL`, `TMPDIR` and the SSL certificate variables, and nothing else: activation scripts, provider keys and ambient CUDA/HF/Torch variables stay in Nodecode. What a backend needs (a cache location, a device) belongs in the CLI's own config.

The image is read from disk, so the model names an absolute path; with `comfy3d` that file is uploaded to the ComfyUI server the CLI's config names. An image is at most 20 MiB, PNG, JPEG or WebP, and must already be foreground-masked and square-composited on black — the cell removes no background.

### What the rewrite leaves out

The original forks a detached supervisor per job that outlives the harness. Here the job runs on a thread of Nodecode's own image: a Nodecode that exits mid-job leaves a `running` receipt that is reported as it stands, and the remote run is never cancelled by the local deadline. Also left out: the per-call native knobs for shape resolution, atlas size, camera angle and mesh repair (a native job asks for `native_preset` — `quality-1536-solid768` or `fast` — and the CLI resolves it), the `REMESHER_SETTINGS` file and `REMESHER_*` environment settings (the config section is the one authority), and the plugin's setup skill, whose `native-setup` walk the table above documents instead.

## License

Apache-2.0. The four tools, their arguments and the job contract are remesher's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
