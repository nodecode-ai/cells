# nodecode-imagegen

Image Gateway: generate and edit pictures through any OpenAI Images-compatible gateway, as the `generate_image` tool the model calls and `/image` for you. A Nodecode rewrite of [duu261/hermes-imagegen-openai-compatible](https://github.com/duu261/hermes-imagegen-openai-compatible) by [duu261](https://github.com/duu261).

The original is an image provider for another agent. Here the same two endpoints are a cell of their own: one tool, one command, and the pictures written under a folder you name.

| Surface | What it does |
| --- | --- |
| `generate_image` | One picture from a prompt: subject, setting, style, light, framing. The answer names the file, its size in bytes and its pixel size. |
| `generate_image` with `images` | An edit: up to 16 source pictures - local paths or https URLs - and a prompt that says what to change. |
| `/image` | The same from your side of the keyboard: `/image a red fox in snow`, with `--landscape`, `--portrait`, `--square`, `--model`, `--quality` and `--image PATH` in front. Typed alone it asks for the prompt. |

Aspect picks the size the GPT image models take: landscape 1536x1024, square 1024x1024, portrait 1024x1536. A model the gateway defines itself (a Gemini image model behind the gateway, Flux, anything the gateway names) is sent verbatim with no size and no quality, because gateways reject enum values they do not know.

## Get it

Tell the agent "add the imagegen cell", or run `nodecode add nc://imagegen`.

## Set it up

You need a gateway that speaks the OpenAI Images API - [CLIProxyAPI](https://github.com/router-for-me/CLIProxyAPI), [New API](https://github.com/QuantumNous/new-api), or your own - and its API root. Then, in `/settings` (or in the `imagegen` section of `config.jsonc`):

```jsonc
"imagegen": {
  "base_url": "http://127.0.0.1:8317/v1",
  "api_key_env": "OPENAI_COMPAT_IMAGE_API_KEY",
  "model": "gpt-image-2",
  "quality": "medium",
  "output_dir": "~/.nodecode/images"
}
```

| Setting | What it does |
| --- | --- |
| `base_url` | The gateway's API root, usually ending at `/v1`. HTTPS unless the host is loopback, LAN, Tailscale, CGNAT or IPv6 ULA: the key never crosses the public internet in clear text. |
| `api_key_env` | The name of the environment variable holding the gateway key, never the key itself. Leave it naming an unset variable for a keyless gateway. |
| `api_key_file` | A file holding the key instead, which the file's own permissions guard. |
| `model` | The model id the gateway serves for images. On the GPT image models a quality may ride the name: `gpt-image-2-high`. |
| `quality` | `auto`, `low`, `medium`, `high`, `xhigh` or `max`, sent for the GPT image models only. |
| `output_dir` | Where pictures are written, `~/.nodecode/images` unless you say otherwise. |
| `timeout` | How long one request may take, in seconds. A big render is slow. |

## Use it

You do not call the tool; the model does. "Draw a wide shot of a red fox in snow", "edit ~/pictures/fox.png and make the sky dusk", "make a portrait illustration of a heron in ink". A person can call the same function from eval, in the `imagegen` package:

```lisp
(imagegen:generate-image "a red fox in snow" :aspect "landscape")
```

`(help :imagegen)` is the model's manual for the tool, kept while the cell runs.

### What it sends, and what it will not

The prompt, the model, the size and quality, and any source images go to the `base_url` you set, and to nowhere else. Nothing else is read: no other app's tokens, no browser profile, no environment beyond the one variable you name. A source image must really be a PNG, JPEG, WebP or GIF - checked by its first bytes, at most 50 MB - so a mistyped path fails before any request instead of uploading a text file.

A failed call says the HTTP status, the gateway's own message with your key blanked out, and the hint that usually fixes it: a wrong key, a `base_url` that does not end at the API root, a model the gateway has not enabled, a rate limit, or a proxy that cut a slow render at its timeout.
