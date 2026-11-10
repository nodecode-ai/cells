# nodecode-podcast

Podcast Studio: turns a two-host script into a mastered MP3 episode, one Microsoft Edge voice per host. A Nodecode rewrite of [suhas-svg/hermes_podcast](https://github.com/suhas-svg/hermes_podcast) by [suhas-svg](https://github.com/suhas-svg).

## Get it

Tell the agent "add the podcast cell", or run `nodecode add nc://podcast`.

## Set it up

Nothing is required: the voice service (Microsoft Edge's read-aloud voices) needs no key, and `ffmpeg` is used when it is on `PATH`. The `podcast` section has defaults for both hosts' voices and the pause between turns:

```jsonc
{
  "podcast": {
    "host_a_voice": "en-US-AndrewMultilingualNeural",
    "host_b_voice": "en-US-AvaMultilingualNeural",
    "pause_ms": 350,
    "intro_chime": true,
    "episodes_dir": "~/podcast/",
    "ffmpeg_path": ""
  }
}
```

Without `ffmpeg` the episode is still produced: the clips are joined as they are, with no chime, pauses, loudness pass or embedded chapters, and the answer says so.

## Use it

Ask for what you want to listen to: "make a 4-minute podcast about this paper" or "a daily briefing on AI research". The model reads the sources, writes the script and calls:

- `podcast_produce` — title, description, hosts (1-3) and lines (speaker, text, optional chapter); answers the episode id, duration, chapters, the MP3 and its timestamped transcript;
- `podcast_voices` — the voice list for a locale, and `gender`;
- `podcast_episodes` — what was produced, newest first, or one episode again by id.

`/podcast` lists episodes, `/podcast voices es` lists Spanish voices and `/podcast doctor` says what this machine has. `(help :podcast)` carries the writing guidance the original shipped as a skill: read the sources first, two hosts, 150 spoken words a minute, no markdown read aloud.

Every episode is a folder under the episodes directory: `episode.mp3`, `transcript.md` with a timestamp per line, and `episode.json` with the hosts, chapters and duration.
