# nodecode-zero-breach

Zero Breach: check this machine for the traces of known mercenary spyware (Pegasus, Predator, Graphite, Candiru and the rest of the published families), with every check that could not run kept apart from the clean answer.

A Nodecode cell over [interplains/Zero-Breach-Toolkit](https://github.com/interplains/Zero-Breach-Toolkit) by archivegenocide, vendored unmodified at a pinned commit (see `vendor/VENDOR.md`). The scanner is theirs; this cell is the four calls, the words, and the two rules it will not break.

## The two rules

1. **A gap is not a clean result.** A check that could not run is reported as a gap, in the engine's own sentence, in the same answer as the verdict. A green result means "no known indicator was found in what could be read", never "you are clean".
2. **The engine's caveats travel with the answer.** The scanner ships `must_relay` sentences addressed to whoever relays a result, and this cell passes them on verbatim.

## What it asks

The scanner answers two questions: are published indicators present in the places this machine can actually be checked, and what evidence survives, and what could not be checked. It reads browsers, startup items, programs, DNS and trust settings, Wi-Fi and the doors left open, and it writes down what it could not see.

- `zerobreach_scan` - check this machine. `scope` picks quick (the default: browsers, host, startup items - the file fingerprints are skipped and said to be skipped), standard (adds them, the vendor's advice), full (adds raw web caches), browser (profiles only) or targeted (the copies of startup files in a folder you name).
- `zerobreach_phone` - an AndroidQF collection, an Android bug-report `.zip`, or an iPhone backup folder, instead of this computer.
- `zerobreach_last` - re-read the newest saved report. No scan, no process.
- `zerobreach_describe` - what the tool checks, what each check reads and cannot see, the exit codes, and the tool's own rules for an assistant. Answered from the engine, no scan.

`/zero-breach` does the same from the operator's side: `/zero-breach scan standard`, `/zero-breach phone /path/to/backup`, `/zero-breach describe browser`, `/zero-breach last`.

## Get it

Tell the agent "add the zero-breach cell", or run `nodecode add nc://zero-breach`.

## Set it up

One thing is required and it is already on most machines: **Python 3.8 or newer**, standard library only. Nothing is downloaded at scan time and no network is contacted during a scan - the indicator sets ride inside the cell.

Settings (`zero-breach` section):

| setting | what it does |
| --- | --- |
| `scope` | the scope a bare `/zero-breach` uses; `quick` by default |
| `timeout` | seconds one scan may take before it is stopped (1800) |
| `python` | the interpreter that runs the scanner (`python3`) |
| `toolkit` | another copy of the toolkit's `scan` folder; empty means the vendored one |
| `feeds` | use indicator sets downloaded with the toolkit's own `--update` as well (off) |
| `iocs` | one more indicator file to scan with |
| `report_directory` | where the JSON reports are kept; empty means this profile's `zero-breach` folder |

## What a scan costs, measured

On one Linux desktop (Fedora, 7.2 kernel, x86_64, 3,338 browser records examined), the scanner at 3.0.2 with its bundled 3,122-indicator set: **quick took 14 seconds**, **standard took 2 minutes 19 seconds**. A full scan, which adds the raw web caches of desktop apps, is the slow one; a phone collection or an iPhone backup is slower again.

## What it will not tell you

- It looks for **published** traces. Spyware nobody has published indicators for is invisible to it, and the traces this grade of spyware leaves expire within weeks. A cache or cookie that reaches back 88 days cannot cover a campaign from 2024.
- A green result is not a clean bill of health, and a hit is not proof of execution: read the tier and the confidence the engine assigns, and take the caveats with it.
- The full report carries machine paths, profile names and browsing details. It stays on this machine unless someone deliberately shares it; the engine's `--share-json` summary is the one that is safe to send.

## Credits

The scanner, its indicator sets and every check it runs are **archivegenocide's** ([interplains/Zero-Breach-Toolkit](https://github.com/interplains/Zero-Breach-Toolkit), Apache-2.0, built on [interplains/zero-breach](https://github.com/interplains/zero-breach)). The indicator values were published by The Citizen Lab, Microsoft, ESET, Google Threat Intelligence Group, Amnesty International and others; their citations are in the indicator files' own headers. This cell is the Lisp around it and claims none of that work. See `NOTICE` and `vendor/VENDOR.md`.
