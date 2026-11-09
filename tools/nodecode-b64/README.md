# nodecode-b64

B64: decodes base64, base64url, hex and percent-encoded text, and says what it holds - text as text, JSON pretty-printed, gzip and zlib inflated, a binary named by its format with a hex dump. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

## Get it

Tell the agent "add the b64 cell", or run `nodecode add nc://b64`.

## Set it up

Nothing is required: the cell is on from the first message. The `b64` section of the config refines the bounds the original hard-coded - `max_input_chars` (65,536; a longer value is refused, not cut), `max_inflated_bytes` (1 MiB; inflation is cut there), `text_chars` and `text_lines` (3,000 and 60; the rest of a result is counted), `dump_bytes` (128; how much of a binary is dumped). `config.example.jsonc` is the section written out.

## Use it

Base64 turns up everywhere: Kubernetes secrets, `Authorization: Basic` headers, data URIs, queue payloads, compressed log fields. A model decoding it in its head gets bytes wrong often enough to matter, and cannot inflate gzip at all.

- The tool `b64_decode` takes the `value` and, if the shape is ambiguous, the `encoding` (`auto`, `base64`, `base64url`, `hex`, `url`). Ask it to look at a `@b64:` value and the same description the original put in the message comes back as the tool result.
- `/b64 <value>` does it for the operator: `/b64 hex:48656c6c6f` answers `Hello`.
- `(help :b64)` is the manual the model reads when it wants the full rules.

```
SGVsbG8sIHdvcmxkIQ==          base64 (standard or URL-safe, padded or not)
48656c6c6f                     hex (also 0x48656c6c6f, 48:65:6c:6c:6f, \x48\x65...)
caf%C3%A9%20au%20lait          percent-encoding
data:image/png;base64,iVBO...  a data: URI
base64:<value>                 pick the encoding: base64:, base64url:, hex:, url:
```

The encoding is told by the value's shape: a `%XX` sequence means percent-encoding, hex digits in pairs (or a `0x` prefix) mean hex, and the base64 alphabets cover the rest. A value that fits both hex and base64 is read as hex, with a note saying how to read it as base64 instead.

What comes back:

```
base64: 18 characters -> 13 bytes
Content: text, 13 characters, 1 line
Text
  Hello, world!
```

```
base64: 67 characters -> 50 bytes
Content: gzip data, inflates to 32 bytes
Inflated: JSON object, 2 keys
  {
    "event": "deploy",
    "ok": false
  }
```

Text is shown as is up to the section's limits; control and invisible characters (a zero-width space, a bell) are written out as `\x07` / `\u200b` so they cannot hide in plain sight, and UTF-8 and UTF-16 byte order marks are recognised. gzip and zlib data is inflated in memory, one layer deep, and what it held is described the same way. Binary is named when its first bytes give it away (PNG and GIF with their size, JPEG, WebP, PDF, ZIP, gzip, bzip2, xz, zstd, 7-Zip, ELF, Mach-O, Windows PE, Java class, WebAssembly, SQLite, DER, Ogg, MP3, WAV, MP4) and shown as a hex dump of its start. Notes call out a value that was cut short or edited, padding that is off, text that is base64 or percent-encoded again, and a JWT inside. A value that cannot be read answers with the reason, so the model can act on it instead of guessing.

What it does not do: it decodes; it never runs, opens, saves or fetches what it decodes, and it does not decrypt anything. It follows no URL in the decoded text, and a second layer of encoding is described, never decoded by itself.

Security: no network, no subprocess, no file read or write, no credential anywhere. The decoded description becomes part of the conversation - the tool result the model reads and the session history - which is the point, and the reason to decode a secret only where it may be said out loud.

## What is in it

| File | What it is |
| --- | --- |
| `decode.lisp` | The decoder: the encodings, what the bytes hold, the notes, the description. |
| `cell.lisp` | The declaration: the `b64` section, the `b64_decode` tool, `/b64`, `(help :b64)`. |
| `nodecode-b64.asd` | The system and its test system. |
| `test/` | The offline tests: the decoder's half, the tool and the command, the section's bounds. |
| `config.example.jsonc` | The section, written out as documentation. |

## License

MIT. The original is Copyright (c) 2026 olric3015: see `LICENSE` and `NOTICE`.
