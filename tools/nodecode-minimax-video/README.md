# nodecode-minimax-video

MiniMax Hailuo Video: text-to-video, image-to-video and subject-reference video through MiniMax's platform, as the `generate_video` and `video_status` tools and the `/video` command. A Nodecode rewrite of [rriggs/hermes-plugin-minimax-video](https://github.com/rriggs/hermes-plugin-minimax-video) by [rriggs](https://github.com/rriggs).

The original plugs itself into another agent's video-provider lane. Here the same MiniMax video API is a cell of its own: two tools a model calls, one command you type, and the finished videos written under a folder you name.

| Surface | What it does |
| --- | --- |
| `generate_video` | One video from a prompt. With `image` it animates that picture as the first frame; with `reference_images` it animates a character from them. |
| `video_status` | A running task's state, and once it has finished, the video itself - retrieved, downloaded and named. |
| `/video` | The same from your side of the keyboard: `/video a red fox crossing snow, wide shot`, with `--model`, `--duration`, `--resolution`, `--image`, `--reference`, `--wait` and `--no-wait` in front. Typed alone it asks for the prompt. |

A render is asynchronous on MiniMax's side: submit, poll, retrieve, download. `generate_video` waits for it (ten minutes by default, or `wait_seconds` in the call), and a call that runs out of that wait answers the task id instead, which `video_status` picks up later. Nothing is billed for a call refused before the request: an empty prompt, a missing key, a missing image file.

| Model | What it is for |
| --- | --- |
| `MiniMax-Hailuo-2.3` | The default. Text-to-video and image-to-video, 768P or 1080P, 6 or 10 seconds. |
| `MiniMax-Hailuo-2.3-Fast` | A faster image-to-video variant. |
| `MiniMax-Hailuo-02` | The previous generation: 512P, 768P or 1080P. |
| `S2V-01` | Subject-reference-to-video: the character in a reference image is the one that moves. |
| `T2V-01`, `T2V-01-Director`, `I2V-01`, `I2V-01-Director`, `I2V-01-live` | The older models, still accepted by name. |

Duration is 6 or 10 seconds, the only two lengths MiniMax renders; anything else is taken to the nearer one. Camera moves go in the prompt in the brackets MiniMax reads - `[Push in]`, `[Pan left]`, `[Truck right]`, `[Zoom out]`, `[Static shot]`, `[Shake]`, `[Tracking shot]` - and a cut reads `[Pedestal up], then [Static shot]`. Two moves in one bracket happen at once: `[Pan left,Pedestal up]`.

## Get it

Tell the agent "add the minimax-video cell", or run `nodecode add nc://minimax-video`.

## Set it up

You need a MiniMax platform account and an API key: [platform.minimax.io](https://platform.minimax.io) - interface key, under basic information. Then either name the environment variable that holds it, or paste it into the section:

```jsonc
"minimax-video": {
  "api_key_env": "MINIMAX_PAYG_KEY",
  "model": "MiniMax-Hailuo-2.3",
  "resolution": "auto",
  "output_dir": "~/.nodecode/videos"
}
```

| Setting | What it does |
| --- | --- |
| `api` | The API root: `https://api.minimax.io` unless your account is on another region. Plain HTTP is allowed only on a loopback or `.local` host. |
| `api_key` | The key itself, pasted into `/settings`. |
| `api_key_env` | The environment variable holding the key: `MINIMAX_PAYG_KEY` unless you say otherwise. The inline `api_key` wins when both are set. |
| `model` | The model text-to-video and image-to-video use. |
| `model_s2v` | The model subject-reference-to-video uses: `S2V-01`. |
| `resolution` | `auto`, `512P`, `720P`, `768P` or `1080P`. `auto` picks 768P on the Hailuo models and 720P on the older ones. |
| `prompt_optimizer` | Let MiniMax rewrite the prompt before rendering. On by default. |
| `output_dir` | Where downloaded videos are written, `~/.nodecode/videos` unless you say otherwise. |
| `wait_seconds` | How long a call waits for the render before answering the task id. Ten minutes by default; `0` answers as soon as the job is accepted. |
| `poll_seconds` | How long between two checks of a running render. |
| `request_timeout` | One HTTP request's ceiling, in seconds. |

## Use it

Ask in words - "make a six-second video of a paper boat going down a rain-filled gutter" - and the model calls `generate_video`. Or type it:

```
/video --duration 10 --resolution 1080P [Push in] a lighthouse in a storm, then [Static shot]
/video --image ~/pictures/portrait.png --duration 6 make her smile and turn to the camera
/video --reference ~/pictures/hero.png the hero walks out of the fog
/video --no-wait a long slow pan over a desert at dawn
```

The answer names the file it wrote and the size in bytes; a call with `--no-wait`, or one that ran out of its wait, answers a task id instead:

```
video_status {"task_id": "123456789012345"}
```

A local image is sent inline as a base64 data URI, at most 10 MB; anything larger should be an https URL. MiniMax's own download links expire, which is why the cell copies the video to `output_dir` rather than handing the URL on.
