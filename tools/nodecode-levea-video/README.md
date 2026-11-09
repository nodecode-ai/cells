# nodecode-levea-video

Levea video editing: describe the edit in plain language and the cloud editor plans it, verifies it and exports an MP4. A Nodecode rewrite of [brajendrak00068/agentic-ai-video-production](https://github.com/brajendrak00068/agentic-ai-video-production) by [brajendrak00068](https://github.com/brajendrak00068).

Five tools, one service. `levea_video_edit` takes one natural-language edit: trim, 9:16 vertical reframe, clips, animated captions, B-roll, multicam podcast sync, audio cleanup, music, voiceover, colour grading, motion graphics, MP4 export. `levea_queue_edit` queues a long edit against a project, `levea_task_status` reads it back, `levea_job_status` follows an export or render job to its end, and `levea_active_task` reconnects to the edit still running on a project after a dropped call.

## Get it

Tell the agent "add the levea-video cell", or run:

```sh
nodecode add nc://levea-video
```

## Set it up

Generate a Levea API key at <https://livecore.ai/> with a Levea account, then either paste it into the cell's `api_key` in `/settings`, or export it:

```sh
export LEVEA_API_KEY=...
```

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `api_key` | (unset) | the key, written inline as a secret |
| `api_key_env` | `LEVEA_API_KEY` | the variable read when `api_key` is empty |
| `api_url` | `https://api.livecore.ai` | the editor's host; the product's web host and a full API path are both accepted, and plain `http` only for a localhost stand-in |
| `project_id` | (unset) | the project a call continues when it names none |
| `timeout_seconds` | `600` | one call's ceiling, and how long `wait: true` polls |
| `poll_seconds` | `4` | how often `wait: true` reads the job again |

Without a key the tools still answer, with the sentence that says which setting to fill, and nothing is billed.

## Use it

Ask for the edit you want, in one sentence, and give the source: "cut this podcast into a 30 second 9:16 reel around the strongest hook, add bold animated captions, and export MP4". The model calls:

| Tool | Arguments |
|---|---|
| `levea_video_edit` | `prompt`, `video_url` or `assets`, optional `project_id`, caption, brand and approval fields |
| `levea_queue_edit` | `prompt` and `project_id`, plus the same source and caption fields |
| `levea_job_status` | `job_id`, optional `wait` |
| `levea_task_status` | `task_id` |
| `levea_active_task` | `project_id` |

The answer leads with the state, the backend's own message, the `project_id`, the `video_url`, `job_id` or `task_id` it produced, and then the whole backend object as JSON. Read `success` and `status`, never the fact that the call returned:

- `success: true` means the backend verified the edit; report `videoUrl` when it is there.
- `pending` with a `jobId` means call `levea_job_status` with `wait: true` for the rendered file.
- `awaiting_approval` means show the plan, then resend with `workingMemory` to continue.
- `partial` or `error` means say what `message` says, and never claim success.

`/levea` shows the host, whether a key is set, the default project and the timeouts; `/levea <project id>` reconnects to the edit running on that project. `(help :levea)` carries the workflow above as the model reads it.

**Cost and privacy.** Every edit and export is billed to your Levea account, and a long edit holds the turn for minutes (`levea_video_edit`, and `levea_job_status` with `wait: true`, poll up to `timeout_seconds`). What leaves the machine is the prompt, the source URLs and the project ids, sent to `api_url` with your key; source video at the URLs you pass is downloaded and processed by Levea. Nothing else is read, no telemetry, no background work.

## Tests

```sh
.github/test-cell.sh tools/nodecode-levea-video
```

Every HTTP call is stubbed: the suite proves the request shape and its headers, that one prompt becomes one verified edit whose project id and file are in the answer, that a success the backend did not verify is reported as such, that a queued edit without a project is refused before any call, that a wait follows a job to its end, that each status tool reads its own path with an id safely encoded, and that a missing key, a refused status and an unreadable body all answer without a second call.

## Credits

A rewrite of brajendrak00068's Hermes Agent plugin, MIT, at commit `0f11f14511c8b9a030689d1541c28752c1d60a59`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
