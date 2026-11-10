# nodecode-triplescan

Triplescan: audits a folder with Gitleaks, OSV-Scanner and Semgrep and answers, per scanner, PASS, FAIL, NOT_APPLICABLE or UNRESOLVED. A Nodecode rewrite of [dafka007/hermes-security-audit](https://github.com/dafka007/hermes-security-audit) by [dafka007](https://github.com/dafka007).

The work is done by three programs the operator already has: [Gitleaks](https://github.com/gitleaks/gitleaks) for leaked secrets, [OSV-Scanner](https://github.com/google/osv-scanner) for vulnerable dependencies, and [Semgrep CE](https://github.com/semgrep/semgrep) for static-analysis findings. The cell runs each of them against one folder, without a shell and without writing, and normalizes what they printed into one result. Nothing of the original's Python is bundled.

## Get it

Tell the agent "add the triplescan cell", or run `nodecode add nc://triplescan`.

## Set it up

```sh
brew install gitleaks osv-scanner semgrep    # macOS
pipx install semgrep                         # any platform; gitleaks and osv-scanner ship release binaries
```

Any of the three may be missing: the audit still runs, and the missing one is reported as UNRESOLVED with the name of the program it wanted. The programs are found on `PATH`; name a path in the settings when one lives elsewhere.

Two of them talk to the network on their own account, and the cell discloses it rather than hiding it:

- **Semgrep** with the default `semgrep_config` of `auto` fetches its rules from Semgrep's registry and may send Semgrep's own metrics. Point `semgrep_config` at a local rules file or folder (`"~/semgrep-rules.yml"`) to keep a scan offline.
- **OSV-Scanner** may contact OSV's advisory services for its database.

The cell itself uploads nothing and writes nothing to the folder it audits.

## Use it

Ask for an audit of a folder: "run a security audit on this project". The model gets one tool:

| Tool | What it answers |
| --- | --- |
| `triplescan` | `workspace` (required), `scanners` (any of `gitleaks`, `osv`, `semgrep`; all three when absent) and `timeout_seconds`. Answers the plugin's structured result: `type`, `overall`, `coverage_incomplete`, `workspace`, `scanners`, `summary`. |

What a scanner's status means:

| Status | What it is |
| --- | --- |
| `PASS` | The scanner ran and found nothing. |
| `FAIL` | The scanner ran and reported findings. |
| `NOT_APPLICABLE` | There was nothing for it to inspect -- OSV-Scanner's own "no package sources found". |
| `UNRESOLVED` | Coverage cannot be trusted: the program is missing, timed out, printed something unreadable, or failed. |
| `BLOCKED` | The workspace itself is not a folder this machine can read; no scanner ran. |

`overall` is `FAIL`, then `UNRESOLVED`, then `PASS`, in that precedence, and `coverage_incomplete` is true whenever any scanner could not see everything. A `FAIL` can still be incomplete: Semgrep findings beside its analysis errors are a failure with incomplete coverage, so a clean-looking result is never a guess.

The operator's command is `/triplescan`:

```
/triplescan /path/to/project    audit that folder
/triplescan                      audit the folder Nodecode runs in
/triplescan help                 what the statuses mean
```

And from eval: `(triplescan:audit "/path/to/project")` answers the JSON, `(triplescan:report "/path/to/project")` the same audit in words. `(help :triplescan)` has the guidance the model is given.

Gitleaks runs with `--redact=100`, and only the rule, the file, the line and the fingerprint are copied out of its report: a secret's value never reaches the model or the transcript. The scan is a snapshot of the files as they stand -- Gitleaks' `dir` mode, not the repository's history -- and nothing is installed, updated or auto-fixed.

## Settings

The `triplescan` section of `~/.nodecode/config.jsonc` (`config.example.jsonc` shows all of it):

| Setting | Default | What it is |
| --- | --- | --- |
| `gitleaks` | `gitleaks` | The program's name on `PATH`, or its path. |
| `osv_scanner` | `osv-scanner` | The same, for OSV-Scanner. |
| `semgrep` | `semgrep` | The same, for Semgrep. |
| `semgrep_config` | `auto` | Semgrep's `--config`: `auto`, a registry pack such as `p/default`, or a local rules file or folder. |
| `timeout_seconds` | `300` | One scanner's ceiling, 10 to 1800 seconds. A run past it is UNRESOLVED. |
| `max_findings` | `200` | How many findings one scanner reports at most. |
| `ask` | `false` | Ask the operator Allow once or Deny, in the web page or desktop app, before a model call runs the scanners; with nobody there the scan is refused. |
| `ask_seconds` | `120` | How long that question waits for an answer. |

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The three scanner invocations, the reading of their output, the approval hook, `/triplescan`, and the standing paragraph. |
| `package.lisp` | The `triplescan` package: the verbs and the condition every refusal signals. |
| `test/` | Offline tests over stubbed scanners: the argv each one is asked with, a Gitleaks report, an OSV walk, Semgrep's errors, a redacted secret, a missing program, an unusable report, a folder that is not one, and the command. |

## License

MIT. The original is Copyright (c) 2026 dafka007: see `LICENSE` and `NOTICE`.
