# nodecode-image-utils

Image Utils: inspects, resizes, crops, rotates, converts and shrinks one local image file at a time, in the cell's own process. A Nodecode rewrite of [MazenMkhinini/hermes-image-plugin](https://github.com/MazenMkhinini/hermes-image-plugin) by [MazenMkhinini](https://github.com/MazenMkhinini).

## Get it

Tell the agent "add the image-utils cell", or run `nodecode add nc://image-utils`.

## Set it up

No key and no program to install. The section `image-utils` in `~/.nodecode/config.jsonc` is optional; `config.example.jsonc` shows it:

```jsonc
{
  "image-utils": {
    "max_pixels": 64,
    "jpeg_quality": 85
  }
}
```

`max_pixels` is the ceiling an image is opened at, and `jpeg_quality` the quality a conversion or an optimize writes JPEG at.

## Use it

Ask in words — "make this photo 800 pixels wide", "crop it square", "turn it 90 degrees", "convert it to JPEG" — and the model calls one of six tools:

| Tool | What it does |
| --- | --- |
| `image_info` | format, size, samples, alpha, bytes, whether the pixels decode, and a JPEG's EXIF orientation and GPS presence. Writes nothing. |
| `image_resize` | one of width, height or percent, the shape kept; refuses to upscale unless `allow_upscale`, and both dimensions need `allow_distort`. |
| `image_crop` | a box `[left, top, right, bottom]` checked against the bounds, a centred aspect such as `16:9`, or a centred width and/or height. |
| `image_rotate` | counter-clockwise: 90, 180 and 270 exactly, other angles with an expanded canvas; `auto_orient` applies the EXIF orientation first. |
| `image_convert` | PNG, JPEG or BMP; alpha into JPEG or BMP must be declared with `flatten`, laid over `background`. |
| `image_optimize` | re-encodes in the input's own format, optionally capping the longest side. |

Every write lands beside the input as `<stem>_<operation>.<ext>` unless `output_path` names a place, and a target that already holds a file is refused unless `overwrite` and `confirm` are both true — the original's gate, kept. `/image-utils` reports the settings and what has been written; `/image-utils info PATH` answers one file's facts.

Reads are PNG, GIF, WebP and JPEG through the engine's own codecs — the ones a vision attachment goes through — and BMP here. A format none of them opens goes through whichever of ImageMagick, ffmpeg or sips the machine has, and the answer says so when there is none.
