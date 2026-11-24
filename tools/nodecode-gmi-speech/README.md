# nodecode-gmi-speech

GMI Cloud speech: say text into an audio file through GMI's request queue, list the
voices a model declares, and the model ids the key may call. A Nodecode rewrite of
[zkasuran/hermes-gmi-media](https://github.com/zkasuran/hermes-gmi-media) by
[zkasuran](https://github.com/zkasuran).

GMI's queue does not answer with audio: it accepts a job, runs it, and names a URL
when it is done. The cell does that round trip for one utterance, checks every
input against the model's own schema before submitting anything, and answers where
the recording landed.

## Get it

Tell the agent "add the gmi-speech cell", or run

```sh
nodecode add nc://gmi-speech
```

## Set it up

Make a key at <https://console.gmicloud.ai>, then either paste it into the cell's
`api_key` in `/settings`, or export it:

```sh
export GMI_API_KEY=...
```

The defaults are the plugin's: `minimax-tts-speech-2.6-turbo`, then
`inworld-tts-1.5-mini` when no model is chosen, an mp3 unless the model offers
something else, and the recordings under `voice/` in the profile's home. What is
worth setting lives in the `gmi-speech` section, which `/settings` writes:

```jsonc
{
  "gmi-speech": {
    "api_key": "",
    "api_key_env": "GMI_API_KEY",
    "model": "minimax-tts-speech-2.6-turbo",
    "voice": "English_expressive_narrator",
    "speed": "",
    "format": "mp3",
    "directory": "voice/",
    "api_base_url": "https://console.gmicloud.ai/api/v1/ie/requestqueue/apikey",
    "poll_timeout_seconds": 300
  }
}
```

| Setting | Default | What it does |
| --- | --- | --- |
| `api_key` | none | the GMI Cloud key, written in the section |
| `api_key_env` | `GMI_API_KEY` | the variable read when `api_key` is empty |
| `model` | none | the speech model every call uses; empty, the first of the plugin's two GMI accepts |
| `voice` | none | the voice every call speaks in unless it names one; empty, the model's own default |
| `speed` | none | the pace asked for; the model clamps it to its own range |
| `format` | `mp3` | mp3, wav, flac or ogg; a model that cannot write it falls back to one it can |
| `directory` | `voice/` | where the recordings are kept, under the home |
| `api_base_url` | GMI's queue | change it only for a stand-in |
| `poll_timeout_seconds` | `300` | how long one job is followed; 30 to 1800 |

## Use it

- **`gmi_speech_say`** — `{text, voice?, speed?, format?, model?}`. Reads the
  model's schema, builds the payload its parameters name, submits it, follows the
  job and downloads the recording to a fresh file whose extension is the
  container the model declared. The answer names that path: give it to the
  operator.
- **`gmi_speech_voices`** — `{model?, query?}`. The voices the model's live schema
  declares, with the section's own marked; when a model declares none, the two
  names the plugin offered. One schema read, nothing generated.
- **`gmi_speech_models`** — `{query?}`. The ids GMI's queue lists for this key,
  with the queue's own uuid and probe ids dropped. One request.
- **`/gmi-speech`** — the settings; `/gmi-speech voices [model]`,
  `/gmi-speech models [words]`, `/gmi-speech say <text>`.
- **`(help :gmi-speech)`** — the model's page.

Ask it "read this sentence aloud with GMI" and it calls `gmi_speech_say`; ask
which voices the model has and it calls `gmi_speech_voices` first.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, GMI's queue and parameter names as data, the shared helpers |
| `cell.lisp` | the section, the three tools, `/gmi-speech`, `(help :gmi-speech)`, the schema/payload and job logic |
| `test/cell-test.lisp` | lifecycle, every tool and the command, every refusal, on a stubbed `nlk:http` |
| `config.example.jsonc` | the section, commented |

## Why three tools, not a provider

In Hermes the plugin registered itself as the harness's text-to-speech provider,
so every spoken reply went through it. Nodecode's provider seam is models, not
voices: speech in a room comes from the channel kit's `speech` section, which
takes an OpenAI-compatible `base_url`, `model` and `voice`. GMI's queue is not
that wire -- a job is submitted and polled, and the audio arrives as a URL -- so
what a user reaches for here is a recording on disk, the voices a model declares,
and the ids there are: three tools and a command.

## Test

From the cells repository's root, offline:

```sh
.github/test-cell.sh tools/nodecode-gmi-speech
```

## License

Apache-2.0. Copyright (c) 2026 zkasuran: see `LICENSE` and `NOTICE`.
