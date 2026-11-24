# nodecode-acedatacloud-media

Ace Data Cloud media: Suno, Seedance, HappyHorse, GPT Image, Nano Banana and eleven more services as five tools the model calls. A Nodecode rewrite of [AceDataCloud/AceDataCloudMediaHermes](https://github.com/AceDataCloud/AceDataCloudMediaHermes) by [AceDataCloud](https://github.com/AceDataCloud).

One Ace Data Cloud key reaches every service it resells: pictures (Nano Banana, GPT Image, Seedream, FLUX, Qwen Image, Midjourney), videos (Seedance, HappyHorse, Veo, Kling, MiniMax, Wan, Grok Imagine), music and speech (Suno, Fish Audio), and Google web or news search with face landmark analysis. Each generate call is a job the service renders: the cell submits it, waits up to a minute, and answers the media URLs — or the task id to ask about again, which costs nothing.

## Get it

Tell the agent "add the acedatacloud-media cell", or run `nodecode add nc://acedatacloud-media`.

## Set it up

Make an Ace Data Cloud key and put it where the cell reads it:

```
export ACEDATACLOUD_API_KEY=...
```

or paste it into the `acedatacloud-media` section of `~/.nodecode/config.jsonc` as `api_key` (see `config.example.jsonc`). Nothing else is required; `api` already points at `https://api.acedata.cloud`, and `wait_seconds` already says how long a call waits for a render.

Every call spends credits on that key, straight away, with no approval prompt, and the costly options (a pro model, 4K, four pictures, a longer clip) spend more.

## Use it

Five tools, one command:

| Tool | What it makes |
| --- | --- |
| `acedatacloud_image` | a picture, or a Nano Banana edit of one to four reference images |
| `acedatacloud_video` | a video, text-to-video or from a first frame |
| `acedatacloud_audio` | a Suno song, from a description or from title, style and lyric; or Fish Audio speech |
| `acedatacloud_task` | the state of a render that outlived its call, and its URLs once it has finished |
| `acedatacloud_lookup` | Google web or news results with links and snippets; or face landmarks in one image |

Every media tool names its `service` (`nano-banana` is the picture default, `seedance` the video one, `suno` the audio one), and takes that service's own knobs: `size`, `resolution`, `count`, `ratio`, `duration`, `image`, `image_urls`, `title`, `style`, `lyric`, `instrumental`.

From the keyboard:

```
/media a lighthouse at dusk, wide shot
/media --video --service veo a paper boat crossing a puddle
/media --audio --service suno a slow song about rain
/media --news who ships open weight models
```

Typed alone, `/media` offers a form to fill in. `--service`, `--model`, `--size`, `--duration` and `--wait N` (or `--no-wait`) narrow a call.

A call that ran out of its wait answers `pending` and a `task_id`; `acedatacloud_task` finishes it for free:

```
acedatacloud_task {service: "veo", task_id: "...", wait_seconds: 60}
```

The full manual is `(help :acedatacloud-media)` in eval.
