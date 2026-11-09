# nodecode-dashscope-video

DashScope video generation: short clips on Alibaba Cloud's HappyHorse models, from text, an image or a reference image. A Nodecode rewrite of [rriggs/hermes-plugin-dashscope-video](https://github.com/rriggs/hermes-plugin-dashscope-video) by [rriggs](https://github.com/rriggs).

The original is a Hermes backend for its `video_generate` tool: Alibaba Cloud
Model Studio's video models, spoken to over DashScope's async task API. The
cell keeps that work as one tool, `video_generate`, and one verb you can call
through `eval`. A clip takes two to three minutes and is billed to your
Alibaba Cloud account, so one call is one considered prompt.

## Get it

Tell the agent "add the dashscope-video cell", or run `nodecode add nc://dashscope-video`.

## Set it up

A key comes from Alibaba Cloud Model Studio's console. Token-plan keys start
`sk-sp-` and answer on the token-plan endpoint; pay-as-you-go keys start
`sk-ws-` and answer on `https://dashscope-intl.aliyuncs.com`. Put either in
the `dashscope-video` section of `config.jsonc` (or `/settings`):

```jsonc
{
  "dashscope-video": {
    "api_key": "sk-sp-…",              // or leave it out and export QWEN_API_KEY
    "api": "https://token-plan.ap-southeast-1.maas.aliyuncs.com",
    "model_family": "happyhorse-1.1",   // -t2v, -i2v, -r2v are appended
    "duration": 5                        // seconds, 3 to 10
  }
}
```

`key_env` names the environment variable the key is read from, `QWEN_API_KEY`
unless it says otherwise, so a key exported in a shell needs no section at
all. A key is required: without one the cell refuses at start and names the
variable it looked for. `/dashscope-video` answers the endpoint, the key it
read, the model each mode asks for, and the last clip.

## Use it

The model calls `video_generate`; you can call the same work from `eval`:

- `video_generate` with `prompt` — text-to-video. The model id is the family
  plus `-t2v`.
- `video_generate` with `prompt` and `image_url` — image-to-video (`-i2v`).
  The image may be an `https` url, a `data:` uri, or a local path, which the
  cell inlines as a base64 data uri (up to 10 MB).
- `video_generate` with `prompt` and `reference_image_urls` —
  reference-to-video (`-r2v`): the clip keeps the subject of the images you
  name. The family takes one, so the first is used.
- `duration` is 3 to 10 seconds and `aspect_ratio` is `16:9`, `9:16`, `1:1`,
  `4:3` or `3:4`; out-of-range lengths are clamped, an unknown shape is 16:9.
- `model` takes a full model id and overrides the family for that call.

```lisp
(dashscope-video:generate "a lighthouse in fog, slow push in" :duration 5)
(dashscope-video:generate "she walks on" :image-url "~/frames/first.png"
                                          :aspect-ratio "9:16")
```

Both answer the same facts: the path of the saved clip, the url DashScope
answered (it expires; the path does not), the model and mode used, the task
id and the size. The clip is cached under this home's `cache/videos/`.

The model id for a mode resolves in the original's order: an explicit `model`
argument, then the section's `model_t2v` / `model_i2v` / `model_r2v`, then
`model_family` plus the mode's suffix.

## Test it

```bash
NODECODE_SRC=~/nodecode/nodecode .github/test-cell.sh tools/nodecode-dashscope-video
```

Every request is a stubbed `nlk:http` and every wait a stubbed clock, so the
suite reaches nothing and sleeps not at all.

## Gaps

The original also accepted `negative_prompt`, `audio`, `seed` and
`resolution` arguments on its provider; the family supports none of them and
the request payload never carried them, so the cell's tool does not offer
them. It has no model listing and no status command beyond
`/dashscope-video`: the original had none either.
