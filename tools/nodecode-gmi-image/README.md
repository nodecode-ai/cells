# nodecode-gmi-image

GMI Image: pictures from GMI Cloud's Seedream, Gemini and the rest of its live catalog, as the `gmi_image` tool and `/gmi-image`. A Nodecode rewrite of [zkasuran/hermes-gmi-media](https://github.com/zkasuran/hermes-gmi-media) by [zkasuran](https://github.com/zkasuran).

The original is an image backend for another agent, plugged into its one `image_generate` tool. Here the same wire is a cell of its own: one tool, one command, and the pictures written under a folder you name. Every call spends GMI credits.

| Surface | What it does |
| --- | --- |
| `gmi_image` | One picture from a prompt. The answer names the file, its size in bytes and its pixel size, the model, the request id and the model's price. |
| `gmi_image` with `images` | An edit, or a subject taken from pictures already on the web: up to 3 public http(s) URLs, and a prompt that says how they should look in the new picture. |
| `/gmi-image` | The same from your side of the keyboard: `/gmi-image a blue boat on a calm lake`, with `--landscape`, `--square`, `--portrait`, `--model` and `--image URL` in front. Typed alone it asks for the prompt. |

The cell reads the model's own schema before it calls: GMI's catalog declares each model's parameters, their options and bounds, and which inputs it requires. The prompt goes to the parameter named `prompt` or `text`; the aspect goes to `aspect_ratio` when the model has one, and otherwise becomes the nearest of the sizes it offers, so a model can answer a shape other than the one asked for. The picture is downloaded as soon as the job finishes, because GMI's links may expire.

## Get it

Tell the agent "add the gmi-image cell", or run `nodecode add nc://gmi-image`.

## Set it up

You need a GMI Cloud API key from https://console.gmicloud.ai. Then either:

- put it in the environment variable `GMI_API_KEY`, which is what the cell reads unless the section names another variable, or
- name a file holding it in the section's `key_file`.

The section `gmi_image` in `~/.nodecode/config.jsonc` is optional; `config.example.jsonc` shows it:

```jsonc
{
  "gmi_image": {
    "api": "https://console.gmicloud.ai/api/v1/ie/requestqueue/apikey",
    "key_env": "GMI_API_KEY",
    "model": "",
    "output_dir": "~/.nodecode/images"
  }
}
```

`model` left empty lets the cell probe its own preference list - `seedream-5.0-lite`, `gemini-2.5-flash-image`, `gpt-image-2-generate` - and take the first one the key can reach that accepts these inputs; name a model id to pin every call. `output_dir` is where pictures land (`~/.nodecode/images` by default), and `timeout` is how long one queued job may take. With no key set the cell still starts: the tool and the command answer with the setting that names one, and nothing is dialled.

## Use it

Ask for a picture - "draw a blue boat on a calm lake, wide shot" - and the model calls `gmi_image`. The answer carries the file's path under `output_dir`, its size and its pixel size, and the model says where it is.

From the keyboard: `/gmi-image a blue boat`, `/gmi-image --portrait the same boat at night --image https://example.com/boat.png`, `/gmi-image --model seedream-5.0-lite a heron`.

From eval, the same call:

```lisp
(gmi-image:generate-image "a blue boat on a calm lake" :aspect "landscape")
```

A failed call says the HTTP status or GMI's own job error, and the setting that usually fixes it; the key itself never appears in an answer. A job still running is polled until the section's `timeout`; a model that needs a source picture is only chosen when the call names one.

## What the original does that this does not

Nothing of its request shape: the same requestqueue base, the same one-hour schema cache, the same mapping of a prompt and an aspect onto a model's declared parameters, the same aspect-to-nearest-size fallback under a 2048x2048 pixel ceiling, image inputs as public URLs only and at most three, the same submit-then-poll job with its growing delay and deadline, the same outcome media URLs, and the same scrubbing of the key out of every error. It is not a provider in another agent's catalog here; it is a cell with a tool of its own. See [NOTICE](NOTICE).
