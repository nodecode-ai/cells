# nodecode-minimax-image

MiniMax Image: pictures from MiniMax's Image-01 models, as the `minimax_image` tool and `/minimax-image`. A Nodecode rewrite of [rriggs/hermes-plugin-minimax-image](https://github.com/rriggs/hermes-plugin-minimax-image) by [rriggs](https://github.com/rriggs).

The original is an image backend for another agent, plugged into its one `image_generate` tool. Here the same wire is a cell of its own: one tool, one command, and the pictures written under a folder you name.

| Surface | What it does |
| --- | --- |
| `minimax_image` | One picture from a prompt: subject, setting, style, light, framing. The answer names the file, its size in bytes and its pixel size. |
| `minimax_image` with `images` | A subject reference: up to 4 source pictures - local paths, https URLs or data URIs - and a prompt that says how the subject should look in the new picture. |
| `/minimax-image` | The same from your side of the keyboard: `/minimax-image a red fox in snow`, with `--landscape`, `--square`, `--portrait`, `--model` and `--image PATH` in front. Typed alone it asks for the prompt. |

Aspect picks the ratio MiniMax takes: landscape 16:9, square 1:1, portrait 9:16. `image-01` draws from a prompt; `image-01-live` keeps a subject from a source picture, and a call that names one upgrades the model on its own. MiniMax reads 1500 characters of a prompt, and its answer links expire within a day, so the cell downloads the picture as soon as it is drawn.

## Get it

Tell the agent "add the minimax-image cell", or run `nodecode add nc://minimax-image`.

## Set it up

You need a MiniMax API key from https://platform.minimax.io (interface keys). Then either:

- put it in the environment variable `MINIMAX_API_KEY`, which is what the cell reads unless the section names another variable, or
- name a file holding it in the section's `key_file`.

The section `minimax_image` in `~/.nodecode/config.jsonc` is optional; `config.example.jsonc` shows it:

```jsonc
{
  "minimax_image": {
    "api": "https://api.minimax.io",
    "key_env": "MINIMAX_API_KEY",
    "model": "image-01",
    "output_dir": "~/.nodecode/images"
  }
}
```

`api` is the host the key was made on, `model` is `image-01` or `image-01-live`, and `output_dir` is where pictures land (`~/.nodecode/images` by default). With no key set the cell still starts: the tool and the command answer with the setting that names one, and nothing is dialled.

## Use it

Ask for a picture - "draw a red fox in snow, wide shot" - and the model calls `minimax_image`. The answer carries the file's path under `output_dir`, its size and its pixel size, and the model says where it is.

From the keyboard: `/minimax-image a red fox in snow`, `/minimax-image --portrait the same fox, at night --image ~/pictures/fox.png`, `/minimax-image --model image-01 a heron`.

From eval, the same call:

```lisp
(minimax-image:generate-image "a red fox in snow" :aspect "landscape")
```

A failed call says the HTTP status or MiniMax's own `base_resp` code, its message, and the setting that usually fixes it; the key itself never appears in an answer.

## What the original does that this does not

Nothing of its request shape: the same endpoint, the same two models and their upgrade rule, the same three ratios, the same 1500-character prompt, the same 10 MB inline cap for a source image, the same `base_resp` error shape, and the same "download it now, the link expires" cache. It is not a provider in another agent's catalog here; it is a cell with a tool of its own. See [NOTICE](NOTICE).
