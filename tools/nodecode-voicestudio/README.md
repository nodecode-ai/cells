# nodecode-voicestudio

VoiceStudio: speak, design voices, render chaptered audiobooks and dub videos through a local VoiceStudio server. A Nodecode rewrite of [aska-digital/protean-voice-studio](https://github.com/aska-digital/protean-voice-studio) by [aska-digital](https://github.com/aska-digital).

Six tools, and a command for each (and `/voicestudio`, the cell's own): `vs_speak` says a text and names the audio file, `vs_list_voices` lists the voices the server offers, `vs_design_voice` makes a new one from words or from a recording, `vs_longform` renders a chaptered script into an audiobook, `vs_dub` dubs a video into other languages, and `vs_status` says whether the server is ready.

The speech is made by the VoiceStudio app on this machine. The cell only talks to the server it runs, at the address the section names; nothing else is contacted, and the audio it answers is written under the cell's own folder.

## Get it

Tell the agent "add the voicestudio cell", or run:

```sh
nodecode add nc://voicestudio
```

## Set it up

Install and start [VoiceStudio](https://github.com/debpalash/VoiceStudio) and let it finish loading its models. Its server answers on `http://127.0.0.1:3900` unless the app says otherwise; put that address in the section.

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `base_url` | `http://127.0.0.1:3900` | the address the VoiceStudio app answers on |
| `default_voice` | (unset) | the voice a call names none; `/vs-voices` lists the ids |
| `directory` | `voice-studio/` | where the audio files are written, under the home |
| `timeout` | `600` | seconds one generation may take |
| `connect` | `3` | seconds a call waits for the server to answer at all |
| `probe` | `3` | seconds one status or job read waits |
| `poll_interval` | `10` | seconds between two job reads |
| `polls` | `60` | how many times a job is read before the cell answers that it is still going |

Without a server the cell still starts: every tool then answers that it could not reach VoiceStudio and names the address. `vs_status` says when it is back.

## Use it

Ask for it: "say this out loud in the narrator voice", "design a warm young female voice called Ana", "which voices have I got", "render this script as an audiobook", "dub this clip into Spanish and French", "is VoiceStudio ready".

| Tool | Arguments | What a user gets |
|---|---|---|
| `vs_speak` | `text`, `voice`, `speed`, `seed`, `out_path` | the audio file, its size, the voice and the seed that repeats the take |
| `vs_list_voices` | `search`, `kind` | the saved voices, the gallery and the presets, with the ids the other tools take |
| `vs_design_voice` | `name`, `describe` or `sample`, `preview` | the new voice's id, and a rendered preview unless `preview` is false |
| `vs_longform` | `file`, `voice`, `format`, `title`, `author`, `resume`, `seed` | the chaptered file, its format and its chapter count |
| `vs_dub` | `video`, `langs`, `voice` | the file each language made, and which language failed when one did |
| `vs_status` | `detail` | whether the server is ready, its engine, its voice count, its free disk |

Commands take the same arguments as `--flag value`, and quote their text. `/voicestudio` prints the commands beside the settings and the address the cell runs on, and `--status` asks the server itself:

```text
/voicestudio
/voicestudio --status
/vs-speak --text "Ship it." --voice narrator
/vs-design --name Ana --describe "warm young female, slight British accent"
/vs-voices --search warm --kind mine
/vs-longform --file episode.md --format m4b --title "Episode one"
/vs-dub --video intro.mp4 --langs es,fr
/vs-status
```

A script for `vs_longform` is markdown: each `# heading` starts a chapter and `##` stays body text. The markup a voice can perform is small -- `[pause]`, `[pause 500ms]`, `[slow]`, `[fast]`, `[emphasis]`, `[spell]`, `[[word|say-this]]` and `[voice:NAME]`. Anything else (`[sigh]`, `<break/>`, `(pause)`) is refused before the render, with the lines named, because a voice would read it aloud literally.

A render or a dub the server has not finished is reported with its job id: run the tool again with that id in `resume` and nothing is lost.
