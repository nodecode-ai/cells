# nodecode-custodian

Custodian: watches the log files you name for known failure patterns, keeps an issue ledger, and gives the fix for each. A Nodecode rewrite of [indigokarasu/hermes-custodian-plugin](https://github.com/indigokarasu/hermes-custodian-plugin) by [indigokarasu](https://github.com/indigokarasu).

Failures repeat. A job that was disabled after three failures, a rate limit that hit because four jobs started on the same minute, an expired token, a full disk: each one has a log line that names it and a fix that worked. Custodian is that knowledge as a table of 35 fingerprints, so the model reads the log once and knows what it is looking at, instead of starting from the message in front of it.

## Get it

Tell the agent "add the custodian cell", or run `nodecode add nc://custodian`.

## Set it up

Name what to watch. In the cell's `sources` setting (`/settings`, or `"custodian"` in `~/.nodecode/config.jsonc`) list the log files to read, one path each — a directory means every `.log` and `.jsonl` in it:

```jsonc
{
  "custodian": {
    "sources": ["~/.nodecode/logs/gateway.log", "~/.local/state/my-service/"],
    "max_age_hours": 24,
    "tail_lines": 500
  }
}
```

No key and no account: a scan reads the files you named and writes only under `~/.nodecode/custodian/`. A cell with an empty `sources` refuses to start and says so, rather than running mute.

## Use it

The model gets three tools:

| Tool | What it answers |
| --- | --- |
| `custodian_scan` | Every watched file read, each known pattern that matched, the line it matched, and the fix known for it. |
| `custodian_issues` | The ledger: the open issues with their evidence, a count by state and tier, or one issue resolved. |
| `custodian_status` | What is watched, how many fingerprints there are, what is open, when the last scan ran. |

The operator types `/custodian status`, `/custodian scan`, `/custodian issues`, `/custodian issues resolve ID`.

Three things keep it quiet enough to leave on. A finding carries the line that matched, not the top of the file, so it is something to read. A line that merely quotes a pattern — an earlier report about a finding — is not a finding, so a repaired issue does not report itself forever. And a match older than `max_age_hours` is dropped, with a timestamp-less continuation line judged by the last dated line above it, so a three-day-old traceback does not read as live.

A finding is evidence, not a diagnosis: read the line, decide whether the fix applies, apply it, resolve the issue, scan again. Nothing repairs anything by itself.
