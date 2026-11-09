# nodecode-kokoro-tts

Kokoro: a local voice, through a Kokoro server on loopback — text in, a WAV file
out, one voice per sentence's language. A Nodecode rewrite of
[brunosouzas/hermes-kokoro-tts](https://github.com/brunosouzas/hermes-kokoro-tts)
by [brunosouzas](https://github.com/brunosouzas).

Nothing leaves the machine: the cell speaks to the same Kokoro server the
plugin ran, on `127.0.0.1`, and answers where the recording is. With a voice map
one answer can be read in several languages, each sentence in the voice of its
own.

## Get it

Tell the agent "add the kokoro-tts cell", or run `nodecode add nc://kokoro-tts`.

## Set it up

The cell needs a Kokoro server, not a key. The plugin's own
(`hermes kokoro install`, then `hermes kokoro restart`) listens on
`http://127.0.0.1:8770` and serves `/v1/audio/speech` and `/v1/voices`, which is
what the cell dials. Any other Kokoro server speaking that OpenAI-shaped
`/v1/audio/speech` answers too: point `url` at it.

The defaults are the plugin's, so with that server running there is nothing to
set. What is worth setting lives in the `kokoro` section, which `/settings`
writes:

```jsonc
{
  "kokoro": {
    "url": "http://127.0.0.1:8770",   // the server's root
    "voice": "af_heart",              // the default voice
    "voices": ["pt=pf_dora"],         // a language of its own, LANGUAGE=VOICE
    "speed": "1.0",                    // 0.25 to 4.0
    "output_dir": "~/voices"           // where recordings go; unset, the cache
  }
}
```

| Setting | Default | What it does |
| --- | --- | --- |
| `url` | `http://127.0.0.1:8770` | the Kokoro server's root |
| `voice` | `af_heart` | the voice everything speaks with unless something names another |
| `voices` | none | one voice per language, `LANGUAGE=VOICE`: `pt`, `en` and `it` are detected, and `it` only when it has a voice |
| `speed` | `1.0` | how fast the voice speaks, 0.25 to 4.0 |
| `language` | none | single-voice mode: read as this Kokoro language code (`p`, `pt-br`) instead of the voice's own |
| `english_words` | none | English words a non-English voice should say in English |
| `english_accent` | `a` | the accent those loanwords are read with: `a` American, `b` British |
| `output_dir` | the cache | where recordings are written |
| `timeout_seconds` | `120` | one request's ceiling |

A voice is a Kokoro voice id — a language letter, a gender letter and a name
(`af_heart`, `bf_emma`, `pf_dora`, `if_sara`). Anything else is refused at
start, so a voice can never name a file.

## Use it

- **`kokoro_speak`** — `{text, voice?, path?}`. Says the text, one request per
  language run, joins the recordings into one WAV and answers where it is, how
  long it plays and which voices spoke. `path` is where the file goes; unset, a
  fresh file under the cell's cache.
- **`kokoro_voices`** — `{language?}`. What the server holds, each voice with
  the language it reads and which one the section speaks with.
- **`/kokoro TEXT`** — the same, for the operator. `/kokoro` with no text says
  where the server is, what will speak, and the three `speech` settings that
  give a room's spoken replies the same voice.
- **`(help :kokoro)`** — the model's page.

A room's spoken replies already speak through an OpenAI-compatible
`/v1/audio/speech`, so the same server can do both: `speech.base_url`
`http://127.0.0.1:8770/v1`, `speech.model` `kokoro`, `speech.voice` your pick.
The cell and the room then share one voice, and only the cell does the
per-sentence language routing.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the section, the two tools, `/kokoro`, `(help :kokoro)`, the detector and the WAV joiner |
| `test/cell-test.lisp` | lifecycle, both tools, the command and the refusals, every request a stubbed `nlk:http` |
| `config.example.jsonc` | the section, commented |

## Why two tools, not a provider

In Hermes the plugin registered itself as the harness's text-to-speech
provider, and its own server did the speaking. Nodecode's provider seam is
models, not voices: speech in a room comes from the channel kit's `speech`
section, which takes an OpenAI-compatible `base_url`, `model` and `voice` — so
a running Kokoro server is already one configuration line away, and what was
missing is a way to ask for a recording and to see which voices there are.
Those are the two tools. The per-sentence language routing, the voice id rule
and the server's request shape are the original's.

## Test

From the cells repository's root, offline:

```sh
.github/test-cell.sh tools/nodecode-kokoro-tts
```

## License

MIT. Copyright (c) 2026 Bruno Souza: see `LICENSE` and `NOTICE`.
