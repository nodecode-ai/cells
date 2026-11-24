# nodecode-vision-inline

Vision Inline: the session model reads an image itself, and stays looking at it, instead of a captioner's prose standing in for it. A Nodecode rewrite of [BrokeSkill/hermes-vision-inline](https://github.com/BrokeSkill/hermes-vision-inline) by [BrokeSkill](https://github.com/BrokeSkill).

The original is a Hermes middleware that overrides Hermes's `vision_analyze` handler: when the local models.dev cache says the session's model takes image input, the image is attached to the conversation instead of being sent to an `auxiliary.vision` captioner, so follow-up questions need no second look and the model reads the pixels itself. This cell keeps that rule and makes it reachable two ways the engine's own `(look PATH)` verb is not: a tool the model calls by itself, and an image at an http(s) URL.

| Path | When | What the model gets |
| --- | --- | --- |
| inline | `image_input_mode` is `auto` and models.dev says the session's model takes images (or has no row for it) | the image itself, attached behind this call's result and kept in the conversation |
| caption | the model reads text only, or `image_input_mode` is `text` | the answer of the captioning model the section names |

## Get it

Tell the agent "add the vision-inline cell", or run `nodecode add nc://vision-inline`.

## Set it up

Nothing is required while your model takes images: `see_image` attaches the image and you read it directly. A captioner is needed only for the other path — a text-only model, or `image_input_mode` set to `text` — and then `api_base` and `model` are required, and the cell refuses at start until they are there. In `/settings`, or in the `vision-inline` section of `config.jsonc`:

```jsonc
{
  "vision-inline": {
    "image_input_mode": "auto",
    "api_base": "https://api.example.com/v1",
    "model": "your-captioning-model",
    "api_key_env": "VISION_CAPTION_KEY"
  }
}
```

`api_base` is any OpenAI-compatible endpoint (`https://host/v1`); its token comes from `api_key`, or from the environment variable `api_key_env` names. A loopback endpoint may answer without one; a non-loopback one over plain HTTP needs `allow_insecure_http`. `max_tokens` bounds a caption, `max_file_mb` one image from a file or a URL, and `timeout_seconds` one fetch or one caption.

## Use it

`see_image` is what the model calls:

| Argument | What it is |
| --- | --- |
| `image` | a local path, or an `http(s)` URL. PNG, JPEG, GIF or WebP, told by its bytes. |
| `question` | what to ask about it when a captioner reads it; ignored when the image itself is attached. |

Type `/vision` to see which path this session takes and whether a captioner is configured:

```
Vision Inline: image_input_mode auto; this session's model takes images.
An image now goes to the session model inline.
captioner https://api.example.com/v1, model cap-1, token set; max_file_mb 5, timeout 60s, max_tokens 1024.
see_image takes a path or an http(s) URL; /vision PATH sees one at once.
```

`/vision PATH` sees one image at once, by the same rule.

Only the caption path sends the image anywhere, and only to `api_base` with its token. The inline path sends nothing: the bytes go to the model already answering the turn. `(look PATH)` still shows a local file to a model that takes images; `see_image` adds the URL, the tool call, and the captioner fallback.

## Licence

MIT. See [LICENSE](LICENSE).
