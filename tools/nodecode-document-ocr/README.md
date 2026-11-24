# nodecode-document-ocr

Document OCR: reads a page image or a PDF scan into Markdown with the OpenAI-compatible multimodal endpoint you configure. A Nodecode rewrite of [lEWFkRAD/hermes-document-reader](https://github.com/lEWFkRAD/hermes-document-reader) by [lEWFkRAD](https://github.com/lEWFkRAD).

## Get it

Tell the agent "add the document-ocr cell", or run `nodecode add nc://document-ocr`.

## Set it up

The cell needs an OCR endpoint of your own: any OpenAI-compatible server with a multimodal model that can transcribe a page. In `~/.nodecode/config.jsonc`, or in `/settings`, set:

```jsonc
{
  "document-ocr": {
    "api_base": "https://ocr.example.com/v1",
    "model": "your-ocr-model",
    "api_key_env": "OCR_TOKEN"
  }
}
```

- `api_base` is the base URL, HTTPS unless the endpoint is loopback or you set `allow_insecure_http`.
- `api_key` writes the token inline; `api_key_env` names the environment variable that holds it. A loopback endpoint may answer without one.
- `max_tokens` (8192), `timeout_seconds` (120), `max_file_mb` (25), `max_pages` (10) and `render_dpi` (150) bound one call.
- `prompt` replaces the built-in transcription instruction.

`/document-ocr` with no argument shows the endpoint in one line. `document_ocr_probe` asks the endpoint's `/models` whether the address and token work.

## Use it

Ask the agent to read a document — "read scan.png", "what does invoice.pdf say?" — and it calls `document_ocr`. A person can call the same function from eval, in the `nodecode-document-ocr` package:

```lisp
(nodecode-document-ocr::tool-ocr
 (nlk:make-json-object "path" "scan.png" "out" "scan.md"))
```

- `document_ocr {path, prompt?, out?}` — one file read into Markdown. PNG, JPEG, WebP, GIF, BMP and TIFF are sent as they are; a PDF is rendered page by page with `pdftoppm` from poppler (install poppler), at most `max_pages` pages. The recognized text answers the call, and `out` writes it as a file.
- `document_ocr_probe {}` — one authenticated `GET /models`, for when a read failed and the address, token or model name is in doubt.
- `/document-ocr PATH` — the same read from the operator's side; `/document-ocr` alone is the status line.

Only the page image leaves the machine, and it goes only to the endpoint the section names, with that endpoint's token and nothing else. The endpoint sees the document, so choose one you trust with it.

A machine without poppler's `pdftoppm` reads images only; pass a page image, or install poppler.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The whole cell: the section, the base64 encoder, the page reader, both tools, `/document-ocr` and the manual topic `(help :document-ocr)`. |
| `test/cell-test.lisp` | Offline tests: the lifecycle, the refusals, base64 against the RFC 4648 vectors, one page to a stubbed endpoint, a stubbed PDF, the probe and the command. |
| `config.example.jsonc` | The section, commented. |

## License

MIT. The original is Copyright (c) 2026 Jeff Robodie: see `LICENSE` and `NOTICE`.
