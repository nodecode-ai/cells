# nodecode-elevenlabs-music

ElevenLabs Music: writes, generates, edits and splits songs with the ElevenLabs Music API, returning audio files and stems. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

## Get it

Tell the agent "add the elevenlabs-music cell", or run `nodecode add nc://elevenlabs-music`.

## Set it up

The Music API needs a paid ElevenLabs plan. Create a key under **Profile → API keys** at
<https://elevenlabs.io/app/settings/api-keys> (a restricted key needs the **Music** permission) and
put it in the cell's section of `~/.nodecode/config.jsonc`:

```jsonc
{
  "elevenlabs-music": { "api_key": "<your key>" }
}
```

`ELEVENLABS_API_KEY` in the environment is read when `api_key` is empty, so a machine that already
has the key set for another tool needs no config at all. `/settings` or the Cells sheet sets the same
members. Nothing is generated until a tool is called, and the tools refuse with a readable sentence
when the key is missing.

| Setting | Default | What it does |
| --- | --- | --- |
| `api_key` | (unset) | Your ElevenLabs API key; `ELEVENLABS_API_KEY` is the fallback. |
| `music_model` | `music_v2_5` | `music_v2_5`, `music_v2` or `music_v1`. Composition plans, edits and audio references need v2 or v2.5. |
| `output_format` | `mp3_44100_128` | What a song is saved as: MP3 or Opus, 44.1 or 48 kHz. ElevenLabs documents 192 kbps MP3 for its Creator plan and up. |
| `store_for_editing` | `true` | Ask ElevenLabs to keep each song so a later request can edit, extend or sample it (no extra cost). |
| `wait_seconds` | `240` | How long a tool waits for a generation before it answers with a job id. |
| `output_dir` | (unset) | Where tracks are written; empty means the organism's `cache/generated/audio/`. |
| `api_base_url` | `https://api.elevenlabs.io` | Change only for an ElevenLabs data-residency endpoint, e.g. `https://api.eu.residency.elevenlabs.io`. |

## Use it

Ask for a song: "a 40 second upbeat synthwave intro for my podcast", "a sad indie folk song about a
lighthouse, female vocals". The agent calls one of eight tools and the audio lands in
`cache/generated/audio/` as `music_<title>_<timestamp>_<id>.<ext>`, with a `MEDIA:<path>` line it
puts in its reply.

| Tool | What it does | Billed |
| --- | --- | --- |
| `elevenlabs_music_compose` | A song or instrumental from a prompt, 3 seconds to 10 minutes, vocals in many languages | by length |
| `elevenlabs_music_draft_plan` | ElevenLabs writes a composition plan from a prompt: sections, lyrics, styles, durations | free |
| `elevenlabs_music_compose_plan` | Generate from a plan: exact lyrics and structure, and edits of earlier songs (keep some parts, regenerate others, extend, loop, "sounds like this") | by length |
| `elevenlabs_music_upload` | Store your own audio at ElevenLabs so the plan tool can edit, extend or sample it | like a generation |
| `elevenlabs_music_stems` | Split a track or any audio file into 2 stems (vocals, accompaniment) or 6 | by length |
| `elevenlabs_music_from_video` | Background music timed to 1-10 videos, optionally with sound effects | by length |
| `elevenlabs_music_finetunes` | The Music finetunes this account can use | free |
| `elevenlabs_music_job` | Check on, or wait for, a generation still running in the background | free |

A generation takes seconds to minutes. A tool that is still waiting when `wait_seconds` runs out
answers `{status: "running", job_id: ...}`; `elevenlabs_music_job` then waits for it or reports it,
so the turn is never held for a long song. A track id from an earlier result names that track in
`stems` and `upload`.

Every request carries only the prompt, plan or arguments of the call -- plus, for the upload, stems
and video tools, the file you name -- to `api.elevenlabs.io` (or the endpoint you set), with the key
in the `xi-api-key` header. Each track also gets one line in the cell's own `tracks.jsonl` under
`plugin-data/nodecode-elevenlabs-music/`: the track id, kind, title, prompt, model, format, file
name, ElevenLabs' song id and byte count. Nothing in it leaves the machine.

`(help :elevenlabs-music)` teaches the model the same verbs while the cell runs.

## What the rewrite leaves out

The Hermes plugin also draws a music card and a music library in Hermes Desktop, and in chat apps it
arms a background wake so a long song is delivered when it is done. Both are Hermes surfaces and have
no Nodecode counterpart; the tools, their arguments, the endpoints and the files are the same. A
job id also lives as long as the Nodecode process rather than being written to disk for a week, and a
track's length is the requested length rather than measured from the audio frames.

## License

MIT. See `LICENSE` and `NOTICE`: the code is a rewrite of NousResearch's plugin, which is MIT.
