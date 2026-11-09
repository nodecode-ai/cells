# nodecode-empiriolabs-media

EmpirioLabs AI's image, video and speech models as three tools and three
verbs: an image generated or edited, a video made from a prompt or from a
picture, and text spoken into an audio file. A Nodecode rewrite of
[EmpirioLabs-AI/hermes-empiriolabs](https://github.com/EmpirioLabs-AI/hermes-empiriolabs)
by [EmpirioLabs-AI](https://github.com/EmpirioLabs-AI).

## Get it

Tell the agent "add the empiriolabs-media cell", or run
`nodecode add nc://empiriolabs-media`.

## Set it up

Every call needs an EmpirioLabs AI API key from
[the dashboard](https://platform.empiriolabs.ai/dashboard/api-keys). Put it
in the environment:

```sh
echo 'EMPIRIOLABS_API_KEY=…' >> ~/.nodecode/.env
```

or in the cell's section, which `/settings` writes:

```jsonc
{
  "empiriolabs-media": {
    "api_key_env": "EMPIRIOLABS_API_KEY",   // or "api_key" written inline
    "image_model": "flux-2-klein-4b",
    "video_model": "hunyuan-video-1-5",
    "tts_model": "tts-2-flash",
    "voice": "Sarah",
    "video_timeout_seconds": 900
  }
}
```

Without a key the cell still starts, and every verb answers the one thing to
do about it.

| Setting | Default | What it does |
| --- | --- | --- |
| `api_key` / `api_key_env` | none | the key, inline or by environment variable (`EMPIRIOLABS_API_KEY` is read either way) |
| `base_url` | `https://api.empiriolabs.ai/v1` | the API root, for a proxy of your own |
| `image_model` | `flux-2-klein-4b` | the model an image asks for |
| `video_model` | `hunyuan-video-1-5` | the model a video asks for |
| `tts_model` | `tts-2-flash` | the model speech asks for |
| `voice` | none | the voice speech uses when the call names none |
| `timeout_seconds` | `300` | one request's ceiling |
| `video_timeout_seconds` | `900` | how long one video call waits for its job |

## Use it

- **`empiriolabs_image`** — `{prompt, aspect_ratio?, image?}`. One image:
  `square` (1:1), `landscape` (16:9) or `portrait` (9:16). `image` is an
  http(s) URL, a `data:image` URI or a local PNG, JPEG, WebP or GIF of up to
  20 MB to edit or to reference; leave it out for a plain generation.
- **`empiriolabs_video`** — `{prompt, image?, aspect_ratio?, duration?,
  resolution?}`. Text-to-video, or image-to-video when `image` is named.
  Members you leave out are the model's own defaults. The call waits up to
  `video_timeout_seconds`; if the job outlasts it, the answer carries the job
  id and the URL to read it from, and the job keeps running.
- **`empiriolabs_speech`** — `{text, voice?}`. The text spoken into an MP3.
- **`(empiriolabs-media:models :kind "image")`** — the live model catalog,
  one line a model; `"video"` and `"speech"` narrow it.
- **`/empiriolabs-media`** — the operator's way in: alone it says where the
  cell points and whether it has a key; `models [image|video|speech]` lists
  the catalog; `image PROMPT` makes one.

Every answer names the file it saved, under this home's
`media/empiriolabs/`; when a generated file cannot be downloaded the answer
carries the signed URL instead. Every call is billed to the key's account,
with nothing asked first.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the primer, the three tool contracts, `/empiriolabs-media` and the section |
| `media.lisp` | the wire: a request, a job's poll, a download, the file it writes, the four verbs |
| `test/cell-test.lisp` | lifecycle, the three tools, a polled job, an inlined local image, the refusals, every request a stubbed `nlk:http` |
| `config.example.jsonc` | the section, commented |

## Why tools, and not a provider

The original plugin registered three EmpirioLabs AI backends behind Hermes's
own `image_generate`, `video_generate` and `text_to_speech` tools. Nodecode's
core has no media-backend registry a cell can add one to, so each endpoint is
a tool of the cell's own — `empiriolabs_image`, `empiriolabs_video`,
`empiriolabs_speech` — with the same three verbs in Lisp for eval, and
`/empiriolabs-media` and `(help :empiriolabs-media)` beside them.

The original also read the live catalog to negotiate each model's parameters:
the default model, the aspect ratios and sizes a model declares, the voices it
offers, the duration and resolution bounds, the longest text one request
takes. Here a call sends only what the caller names, plus the release
defaults, so a model is never sent a member it does not declare; `models`
still reads the catalog for whoever wants it.

## Test

From the cells repository's root, offline:

```sh
.github/test-cell.sh tools/nodecode-empiriolabs-media
```

## License

MIT. Copyright (c) 2026 EmpirioLabs.ai LLC: see `LICENSE` and `NOTICE`.
