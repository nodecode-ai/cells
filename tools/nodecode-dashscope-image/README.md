# nodecode-dashscope-image

DashScope Image: pictures from Alibaba's Wan 2.7 models through a Qwen Cloud key, as the `dashscope_image` tool and `/dashscope`. A Nodecode rewrite of [rriggs/hermes-plugin-dashscope-image](https://github.com/rriggs/hermes-plugin-dashscope-image) by [rriggs](https://github.com/rriggs).

The original is an image backend for another agent, plugged into its one `image_generate` tool. Here the same wire is a cell of its own: one tool, one command, and the pictures written under a folder you name.

| Surface | What it does |
| --- | --- |
| `dashscope_image` | One picture from a prompt: subject, setting, style, light, framing. The answer names the file, its size in bytes and its pixel size. |
| `dashscope_image` with `images` | An edit or a restyle: up to 4 source pictures - local paths, https URLs or data URIs - and a prompt that says what to change. |
| `/dashscope` | The same from your side of the keyboard: `/dashscope a red fox in snow`, with `--landscape`, `--square`, `--portrait`, `--model` and `--image PATH` in front. Typed alone it asks for the prompt. |

Aspect picks the size the Wan models take, in DashScope's `W*H` form: landscape 1280\*720, square 1024\*1024, portrait 720\*1280. A call that names a source image upgrades `wan2.7-image` to `wan2.7-image-pro` on its own, because the fast model is text-to-image only - the original plugin does the same.

Pictures are downloaded as soon as they are drawn, because the OSS links DashScope answers with expire after about a day.

## Get it

Tell the agent "add the dashscope-image cell", or run `nodecode add nc://dashscope-image`.

## Set it up

You need a Qwen Cloud (Alibaba Cloud Model Studio) key: a token plan key (`sk-sp-*`), a pay-as-you-go key (`sk-ws-*`), or a plain DashScope API key. Put it in an environment variable, then in `/settings` (or in the `dashscope` section of `config.jsonc`):

```jsonc
"dashscope": {
  "api": "https://token-plan.ap-southeast-1.maas.aliyuncs.com",
  "api_key_env": "QWEN_API_KEY",
  "model": "wan2.7-image",
  "output_dir": "~/.nodecode/images"
}
```

| Setting | What it does |
| --- | --- |
| `api` | The DashScope base. The token-plan host is the default; a pay-as-you-go key wants `https://dashscope-intl.aliyuncs.com`. HTTPS unless the host is loopback, LAN, Tailscale or `.local`: the key never crosses the public internet in clear text. |
| `api_key_env` | The name of the environment variable holding the key, never the key itself. Required. |
| `api_key_file` | A file holding the key instead, which the file's own permissions guard. |
| `model` | `wan2.7-image` (fast) or `wan2.7-image-pro` (higher fidelity, takes a source image). |
| `output_dir` | Where pictures are written, `~/.nodecode/images` unless you say otherwise. |
| `timeout` | How long one request may take, in seconds. A render is slow. |

## Use it

You do not call the tool; the model does. "Draw a wide shot of a red fox in snow", "restyle ~/pictures/fox.png with the sky at dusk", "make a portrait illustration of a heron in ink". A person can call the same function from eval, in the `dashscope` package:

```lisp
(dashscope:generate-image "a red fox in snow" :aspect "landscape")
```

`(help :dashscope-image)` is the model's manual for the tool, kept while the cell runs.

### What it sends, and what it will not

The prompt, the model, the size, and any source images go to the `api` base you set, and to nowhere else. Nothing else is read: no other app's tokens, no browser profile, no environment beyond the one variable you name. A local source image is sent base64-inlined, as the original does, because DashScope fetches a URL from its own side; it must really be a PNG, JPEG, WebP or GIF - checked by its first bytes, at most 10 MB - so a mistyped path fails before any request instead of uploading a text file. A picture the answer links is fetched back over https (or plain HTTP only from a local network).

A failed call says the HTTP status, DashScope's own code and message with your key blanked out, and the hint that usually fixes it: a wrong key, an `api` base belonging to another region or plan, a model the key cannot reach, or a rate limit.
