# nodecode-gemini-media

Gemini Media: pictures with Google's Nano Banana image models and clips with Gemini Omni, from a prompt or your own reference pictures. A Nodecode rewrite of [bytecakelake/hermes-gemini-media-generation](https://github.com/bytecakelake/hermes-gemini-media-generation) by [bytecakelake](https://github.com/bytecakelake).

The original is a backend for another agent's image_generate and video_generate lanes. Here the same two Gemini API calls are a cell of its own: three tools a model calls, one command you type, and the media written under a folder you name.

| Surface | What it does |
| --- | --- |
| `gemini_image` | One picture from a prompt: subject, setting, style, light, framing. With `images` it edits, keeps a character, or combines products - up to 14 reference pictures. The answer names the file, its bytes, its pixel size and the model. |
| `gemini_video` | One clip, 3 to 10 seconds, 16:9 or 9:16, 360p to 4k, with Gemini's native audio. `image` makes that picture the first frame; `images` makes the character in them the one that moves; up to 6 in all. The answer names the file, its bytes and its real duration, read out of the MP4. |
| `gemini_models` | The image and video models this key can call, read from the key's own model list, with the curated list behind it when the API cannot be reached. |
| `/gemini-media` | The same from your side of the keyboard: `/gemini-media a red fox in snow --landscape`; `/gemini-media --video --seconds 6 a fox crossing a snowy field`. Typed alone it asks for the prompt. |

| Model | What it is for |
| --- | --- |
| `gemini-nano-banana-2.1` | The default image model: fast, 1K/2K/4K, Google Search grounding, multi-reference. |
| `gemini-3-pro-image` | Nano Banana Pro: complex scenes, text rendering, the highest world knowledge. |
| `gemini-3.1-flash-image`, `gemini-3.1-flash-lite-image` | Nano Banana 2 and 2 Lite: the cheaper image models; the lite one renders 1K only. |
| `gemini-2.5-flash-image` | The legacy Nano Banana, still accepted by id. |
| `gemini-omni-1.1-flash` | The default video model: text, image and reference to video, 3 to 10 seconds, with audio. |

An image is a `response_format` of type image with an aspect ratio and, when you ask for one, an image size; a model that renders one size only is sent the size it renders, and the answer says when it moved. A video is a `response_format` of type video with an aspect ratio, a resolution and a duration the API takes as a string (`"8s"`). A resolution between two supported ones rounds up, and the answer says so.

## Get it

Tell the agent "add the gemini-media cell", or run `nodecode add nc://gemini-media`.

## Set it up

You need a Gemini API key from <https://aistudio.google.com/apikey>. Export it, and tell the cell which variable holds it:

```sh
export GEMINI_API_KEY=...
```

Then, in `/settings` (or in the `gemini-media` section of `config.jsonc`):

```jsonc
"gemini-media": {
  "api_url": "https://generativelanguage.googleapis.com/v1beta",
  "api_key_env": "GEMINI_API_KEY",
  "output_dir": "~/.nodecode/gemini-media"
}
```

| Setting | Default | What it does |
| --- | --- | --- |
| `api_url` | (required) | the Gemini API root. A bare host gains `/v1beta`; plain `http` only for a loopback stand-in. |
| `api_key` | (unset) | the key itself, pasted into `/settings`; a secret the section keeps. |
| `api_key_env` | `GEMINI_API_KEY` | the variable read when `api_key` is empty. `GOOGLE_API_KEY` is read when that one is unset. |
| `api_key_file` | (unset) | a file holding the key, which the file's own permissions guard. |
| `image_model` | `gemini-nano-banana-2.1` | the image model a call gets when it names none. |
| `video_model` | `gemini-omni-1.1-flash` | the video model a call gets when it names none. |
| `output_dir` | `~/.nodecode/gemini-media` | where the pictures and clips are written. |
| `image_timeout` | `150` | seconds one image request may take. |
| `video_timeout` | `300` | seconds one render may take. The video side stalls now and then; a timeout is an error, never a silent retry that bills twice. |

Without a key every call answers with the variable to set; nothing is billed for a call refused before the request - an empty prompt, a missing key, a reference picture that is not a picture.

## Use it

Ask in words - "draw a red fox in snow, wide shot", "make this photo look like dusk", "animate this picture, slow push in, 6 seconds" - and the model calls one of the tools. A render is slow (seconds for a picture, tens of seconds to minutes for a clip), so tell the agent to say it started before it waits.

The cell sends the prompt, the reference pictures and the key to `generativelanguage.googleapis.com` (or the `api_url` you name) and nowhere else. It reads only the picture files you pass, writes only under `output_dir`, runs no shell command and no self-update, and keeps no copy of the key.
