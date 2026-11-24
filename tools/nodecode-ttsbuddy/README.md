# nodecode-ttsbuddy

TTS Buddy: speaks text into an MP3 through the hosted TTS Buddy API, lists the voices offline, and downloads a job that was already submitted. A Nodecode rewrite of [ngelik/ttsbuddy-cli](https://github.com/ngelik/ttsbuddy-cli) by [ngelik](https://github.com/ngelik).

## Get it

Tell the agent "add the ttsbuddy cell", or run

```sh
nodecode add nc://ttsbuddy
```

## Set it up

Make an agent key on your TTS Buddy account, then either paste it into the cell's `api_key` in `/settings`, or export it:

```sh
export TTSBUDDY_API_KEY=...
```

Pick a voice with `ttsbuddy_voices` (or `/ttsbuddy voices French female`) and keep its id in `voice`. The original plugin needed the `ttsbuddy` command-line tool installed; this cell needs nothing but the key, because it posts to the same endpoint the tool posted to.

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `api_key` | (unset) | the agent key, written inline as a secret |
| `api_key_env` | `TTSBUDDY_API_KEY` | the variable read when `api_key` is empty |
| `voice` | `st_m1` | the voice every call speaks in unless it names one |
| `speed` | (unset) | the pace, 0.5 to 1.5, as a number |
| `language` | (unset) | the language a Supertonic `st_*` voice speaks; Kokoro voices take none |
| `directory` | `voice/` | where the MP3s are kept, under the home |
| `api_base_url` | `https://www.ttsbuddy.com/v1/agent-tts` | the agent endpoint; change it only for a stand-in |
| `poll_timeout_seconds` | `600` | how long one job is followed, 30 to 1800 |

Every synthesis is billed to the TTS Buddy account the key belongs to, and uses its minutes. Nothing is sent until a call names a voice and a key.

## Use it

The model says a sentence and gets back the path of the MP3 it may hand you:

```text
ttsbuddy_speak(text, voice?, speed?, language?)
ttsbuddy_voices(query?, language?, limit?)
ttsbuddy_recover(job_id, path?)
```

`ttsbuddy_speak` submits the text, follows the job while it runs, and downloads the finished MP3 to a new file — it never overwrites one. At most a paragraph or two per call: the file is played, not read.

`ttsbuddy_voices` is the curated catalogue the CLI carried, so it costs no request and no key: the Kokoro voices, each speaking its own language, and the ten Supertonic voices that take a `language`.

`ttsbuddy_recover` takes the exact job id an earlier answer named — after a timeout, say — and downloads that job to a fresh path. It never submits speech, so nothing is billed twice. A job that is still running keeps running.

From the operator's side, `/ttsbuddy` shows the settings, `/ttsbuddy voices <words>` searches the catalogue and `/ttsbuddy say <text>` speaks a line.

A local timeout stops the waiting, not the hosted job: if the answer named a job id, `ttsbuddy_recover` with that id is the way back.
