# nodecode-gmi-video

GMI Video: make a short video from a text prompt or a public image URL with GMI Cloud. A Nodecode rewrite of [zkasuran/hermes-gmi-media](https://github.com/zkasuran/hermes-gmi-media) by [zkasuran](https://github.com/zkasuran).

[GMI Cloud](https://www.gmicloud.ai) serves Hailuo, Kling and other video
models behind one request queue. The original plugin plugs them into
Hermes's video picker; a Nodecode has no video lane, so this cell gives the
model the same thing as two tools — `gmi_video` and `gmi_video_models` — and
the same two calls through `eval`, `(gmi-video:generate …)` and
`(gmi-video:models)`. Each generation reads the chosen model's live schema
first, so duration, resolution, aspect ratio and audio use only values that
model accepts; anything it does not take is dropped and named in the answer.

## Get it

Tell the agent "add the gmi-video cell", or run `nodecode add nc://gmi-video`.

## Set it up

A key comes from [console.gmicloud.ai](https://console.gmicloud.ai). Put it in
the `gmi-video` section of `config.jsonc` (or `/settings`):

```jsonc
{
  "gmi-video": {
    "api_key": "…",                  // or leave it out and export GMI_API_KEY
    "model": "Minimax-Hailuo-02",       // text-to-video default
    "image_model": "Minimax-Hailuo-2.3-Fast", // image-to-video default
    "timeout_seconds": 900               // how long one job may take
  }
}
```

`api_key_env` names an environment variable instead, and `GMI_API_KEY` is
read either way, so a key exported in a shell needs no section at all.
`output_dir` says where a finished video is saved; it is the organism's
`cache/gmi-video` unless answered.

**Every generation spends GMI credits, with no approval prompt**, and most
video models bill per second of output: the answer states the price GMI
supplies for the model that ran, but no total is computed.

## Use it

The model calls the tools by name; you can call them too, through
`(help :gmi-video)` and `eval`:

- `gmi_video` — `prompt` (required), `image_url`, `model`, `duration`,
  `aspect_ratio`, `resolution`, `negative_prompt`, `seed`. It submits the
  job, polls it, saves the video and answers its path, the model, the
  duration, the request id and the price.
- `gmi_video_models` — GMI's live video catalog: model id, price, and a
  short description each. The catalog is read once an hour.
- `(gmi-video:generate "a blue boat at dusk" :duration 5)` — the same call
  from eval.
- `(gmi-video:models)` — the same list from eval.
- `/gmi-video` — the list, from the slash line.

Image-to-video takes a **public http(s) URL**, never a local path: upload
the image first and pass its address. Text jobs and image jobs default to
different models. A job that fails or times out comes back as an `ERROR:`
line the model can act on, with the request id to check in GMI's console.

## What it does not do

The original registers itself as a Hermes video backend, so `/video` and its
picker drove it; here the model asks for the tool. Its Hermes-only setup
schema, its model picker and its thread-pool catalog refresh are gone with
that harness; the rewrite maps parameters against the same live schema, one
model at a time. See [NOTICE](NOTICE).
