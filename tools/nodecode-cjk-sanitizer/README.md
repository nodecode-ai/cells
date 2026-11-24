# nodecode-cjk-sanitizer

CJK Sanitizer: strips the Chinese and Cyrillic characters a Chinese-trained model leaks into its answer. A Nodecode rewrite of [evanyudis/cjk_sanitizer](https://github.com/evanyudis/cjk_sanitizer) by [evanyudis](https://github.com/evanyudis).

## Get it

Tell the agent "add the cjk-sanitizer cell", or run `nodecode add nc://cjk-sanitizer`.

## Set it up

Nothing is required: enabled, it strips from the first answer. The `cjk-sanitizer` section of the config refines it — `strip_chinese`, `strip_cyrillic`, `strip_punctuation`, `collapse_space`, `blank_lines` (the most consecutive newlines kept, 2 by default), `strip_reasoning` (the thinking stream too, off by default). `config.example.jsonc` is the section written out.

## Use it

Ask a model served by a Chinese provider something and watch its answer: where a Chinese-trained model drifts into hanzi mid-sentence, or into Cyrillic in a long reasoning chain, the characters never arrive. Nothing else about the answer changes — Korean Hangul, Japanese kana and every Latin word are as the model said them, and the strip happens on the stream, so a shell, a channel, a transcript and the history the next round reads back all see the cleaned text.

- `/cjk` reports the settings and what this process has stripped — deltas read, characters taken out, deltas dropped whole.
- `/cjk off` pauses it until `/cjk on` or a restart; `/cjk reset` clears the counters.
- The tool `cjk_strip` cleans text handed to it: `cjk_strip` with the text and, optionally, which scripts to take out.

What it does not do: it does not touch the text of a tool result, a recalled memory or a file the model read — only what the model itself says. A leak the provider splits across two fragments is cleaned per fragment, and the whitespace tidying is per fragment for the same reason. A Japanese answer that uses kanji loses them too: the CJK unified ideographs are the block Chinese and Japanese share, which is why the original's own "Japanese is preserved" claim only ever held for kana.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The ranges, the pure sanitizer, the advice on the stream, `/cjk` and `cjk_strip`. |
| `nodecode-cjk-sanitizer.asd` | The system and its test system. |
| `test/` | The offline tests: the ranges and the tidying, the advice on a fragment, the command, the tool. |
| `config.example.jsonc` | The section, written out as documentation. |

## License

MIT. The original is Copyright (c) 2026 Evan Yudistira: see `LICENSE` and `NOTICE`.
