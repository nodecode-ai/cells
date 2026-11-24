# nodecode-doppler-geo

Aircraft Doppler Geolocation: finds the flyover tones in a recording and estimates where it was made. A Nodecode rewrite of [mojomast/hermes-aircraft-doppler-geolocation](https://github.com/mojomast/hermes-aircraft-doppler-geolocation) by [mojomast](https://github.com/mojomast).

A plane passing overhead leaves a faint tone in the spectrogram that falls away
either side of closest approach. How far it drops is the plane's speed; how
sharply is how near it passed. Two tools read that:

| Tool | What it answers |
| --- | --- |
| `doppler_detect` | The flyovers in a WAV file: closest approach, speed, slant distance, rest tone, fit residual. Offline. |
| `doppler_locate` | The ground area they and an ADS-B track CSV agree on: best point, top 5, 68/95% areas, a confidence flag. |

## Get it

Tell the agent "add the doppler-geo cell", or run `nodecode add nc://doppler-geo`.

## Set it up

Nothing is required to detect. `doppler_locate` reads ADS-B from a CSV you
give it, with `time_s` in the recording's own clock:

```csv
icao,time_s,lat,lon,alt_ft
abc123,0.0,51.5071,-0.1200,984.3
```

Point the `tracks` member of the `doppler-geo` section at one file (it is then
the default for every call), or name one per call. The rest of the section has
defaults that hold: `fmin` 60, `fmax` 400, `max_events` 3, `sigma_hz` 2,
`sound_speed` 343.

Only RIFF/WAVE is read (PCM 8/16/24/32 or 32-bit float, any channel count).
Convert anything else first: `ffmpeg -i clip.mp4 -ac 1 -ar 8000 clip.wav`.

## Use it

Ask in words — "find the flyovers in this recording" — or type:

- `/doppler clip.wav` — the flyovers.
- `/doppler locate clip.wav 51.5,-0.12,4` — the area, within 4 km of that point.

From eval the same two are `(doppler-geo:detect "clip.wav")` and
`(doppler-geo:locate "clip.wav" "51.5,-0.12,4")`.

Read the confidence first. `ok` needs a posterior that concentrates and a fit
under 2 Hz; a `low` result is a search region, not a point. Over water and
away from ADS-B coverage there are no tracks to score at all.

## What it does not do

The original fetched its own ADS-B: a day of adsb.lol history, about 3 GB
streamed per uncached day, cropped to the region and cached, plus Copernicus
DEM tiles and Open-Meteo temperature. This cell reads the tracks you hand it
and takes ground elevation as sea level. It has no broadband (ground-reflection
jet) mode, no PNG plots, no ffmpeg. See `NOTICE`.

## License

MIT. The method and the original implementation are Copyright (c) 2026 Kyle
Durepos: see `LICENSE` and `NOTICE`.
