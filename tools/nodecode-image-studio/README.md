# nodecode-image-studio

Image Studio: draw pictures with FAL or Vertex AI, keep every one in a gallery, and list what you drew. A Nodecode rewrite of [phdigital01/imagegen-hermes](https://github.com/phdigital01/imagegen-hermes) by [phdigital01](https://github.com/phdigital01).

The original is a desktop studio for another agent, with a provider registration and a command line behind it. Here the same two wires are a cell of their own: two tools the model calls, one command for you, and every picture written under a folder you name.

| Surface | What it does |
| --- | --- |
| `generate_image` | One picture from a prompt: subject, setting, style, light, framing. The answer names the file and its size. |
| `generate_image` with `references` | Draws from pictures you already have: https URLs on either provider, local paths too on Vertex AI. |
| `image_gallery` | What the cell drew, newest first: path, prompt, provider, model and when. |
| `/image-studio` | The same from your side of the keyboard: `/image-studio a red fox in snow`, with `--provider`, `--model`, `--aspect`, `--count` and `--ref PATH` anywhere on the line. Typed alone it asks for the prompt and shows the newest files. |

FAL goes through its queue - one submission, a status URL, a result URL - and Vertex AI through one `generateContent` call. Count, aspect ratio and resolution go to the models that take them; anything else picks its own.

## Get it

Tell the agent "add the image-studio cell", or run `nodecode add nc://image-studio`.

## Set it up

You need a key for at least one of the two providers, named - never pasted into the folder - in the `image-studio` section of `config.jsonc`:

```jsonc
"image-studio": {
  "provider": "fal",
  "fal_key_env": "FAL_KEY",
  "vertex_key_env": "VERTEX_API_KEY",
  "vertex_project": "",
  "output_dir": "~/.nodecode/image-studio"
}
```

| Setting | What it does |
| --- | --- |
| `provider` | `fal` or `vertex`: what a call draws with when it names none. Each has its own key. |
| `fal_key_env` / `fal_key_file` | Where the FAL key is: the environment variable, or a file whose own permissions guard it. |
| `vertex_key_env` / `vertex_key_file` | The same for the Google Cloud API key. |
| `vertex_project` | The Google Cloud project id, when the key is bound to a service account. Blank for an express-mode key. |
| `model` | The model both tools use when a call names none. Blank means `fal-ai/flux-2-pro` on FAL and `gemini-nano-banana-2.1` on Vertex AI. |
| `output_dir` | Where pictures and the gallery index are written. `~/.nodecode/image-studio` unless you say otherwise. |
| `gallery_cap` | How many pictures the gallery lists. Past it the older entries leave the index; their files stay. |
| `timeout` | How long one provider request may take, in seconds. A big render is slow. |

## Use it

You do not call the tool; the model does. "Draw a wide shot of a red fox in snow", "make a portrait illustration of a heron in ink on FAL", "take cover.png and make the sky dusk" (local references are Vertex AI's; FAL draws from https URLs). A person can call the same function from eval, in the `image-studio` package:

```lisp
(image-studio:generate-image "a red fox in snow" :aspect-ratio "16:9")
(image-studio:gallery 5)
```

`(help :image-studio)` is the model's manual for both tools, kept while the cell runs.

### What it sends, and what it will not

The prompt, the model, the shape, the count and any reference images go to the provider you chose, and to nowhere else. Nothing else is read: no other app's tokens, no browser profile, no environment beyond the variables you name. A refusal names the provider, the HTTP status and the provider's own message, and never the key; a FAL result URL must be on `queue.fal.run` before it is followed.

Pictures are only ever written under `output_dir`: a file that cannot be seen there is not touched, and trimming the gallery moves nothing on disk.
