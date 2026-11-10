# nodecode-detox

Tool Output Firewall: huge tool results arrive as a compact report, and their full text stays under your home for exact-line retrieval. A Nodecode rewrite of [kellycryptos/hermes-detox](https://github.com/kellycryptos/hermes-detox) by [kellycryptos](https://github.com/kellycryptos).

## Get it

Tell the agent "add the detox cell", or run `nodecode add nc://detox`.

## Use it

Nothing to set up: the cell is on as soon as it is installed. A tool call that answers more than 6000 characters - a test run, a build log, a directory listing, an API payload - comes back to the model as a report instead:

```
=== [DETOX: sh] (tests) ===
Original output (18,042 chars, 806 lines) kept as evidence ev_1a2b3c4d5e6f.

--- Extracted summary ---
tests: 3 failed, 42 passed
failing lines: 3

--- Head (lines 1-5) ---
   1 | ...

--- Diagnostic findings (4) ---
  43 [ERROR] | error: cannot find symbol `build'

--- Tail (lines 802-806) ---

--- Retrieval ---
detox_inspect(evidence_id="ev_1a2b3c4d5e6f", start_line=<N>, end_line=<M>) answers exact lines.
detox_search(evidence_id="ev_1a2b3c4d5e6f", query="<phrase>") finds a phrase with its line number.
=== [END DETOX REPORT ev_1a2b3c4d5e6f] ===
```

The line numbers in a report are the original's own, so a number read there is a number `detox_inspect` takes unchanged. The model has three tools - `detox_inspect`, `detox_search`, `detox_status` - and you have one command:

```
/detox                     what the firewall has done; what is kept
/detox inspect ID [N] [M]   lines N to M of the record ID
/detox search PHRASE [ID]   where PHRASE appears, with record and line number
/detox purge [DAYS]         drop records older than DAYS
```

The section `detox` in `~/.nodecode/config.jsonc` (or `/settings`) tunes it:

| Key | Default | What it does |
| --- | --- | --- |
| `enabled` | `true` | intercept at all |
| `min_size` | `6000` | a longer result, in characters, becomes a report |
| `max_report_chars` | `3000` | a report longer than this is cut back, or dropped |
| `max_report_percent` | `55` | a report must be at most this percent of the original |
| `head_lines` / `tail_lines` | `5` / `5` | how much of the start and the end a report shows |
| `excluded_tools` | none | tools never intercepted, beside this cell's own three and the file editors |
| `retention_days` | `30` | records older than this go at start and on `/detox purge` |
| `max_store_mb` | `100` | past this, the oldest records go |

A result smaller than `min_size`, an excluded tool, an image or a base64 payload, and a result that already carries a report are left exactly as they were. A report that does not come out small enough is dropped and the original answer stands: nothing here may lose a tool result, so every path fails open.

Everything stays on your machine. A record is two files in the `detox` folder under the home - the text and its one-line metadata - written with no database and no dependency beyond the image. They are **not encrypted**, and anything running as your user can read them; retention and `max_store_mb` bound them. Known secret shapes (`sk-...`, `ghp_...`, `xox?-`, `AKIA...`, `Bearer ...`, a `password=` assignment, a PEM block) are redacted before a record is written, by pattern - a secret of a shape the patterns do not know is kept as it arrived. `detox_inspect` answers by evidence id, and an id is random, but treat one as sensitive: whoever knows it can read that record.

## What the rewrite leaves out

See [NOTICE](NOTICE).

## Licence

MIT, the original's. See [LICENSE](LICENSE).
