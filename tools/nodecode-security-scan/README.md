# nodecode-security-scan

Security Review: reviews a local repository for security flaws with evidence, coverage and Markdown, JSON, CSV and SARIF reports. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

## Get it

Tell the agent "add the security-scan cell", or run `nodecode add nc://security-scan`.

## Set it up

Nothing is required to review a repository: the cell starts on its defaults and
reads a `security-scan` section of `~/.nodecode/config.jsonc` when one is there
(`config.example.jsonc` shows it). Two members are worth knowing:

- `allowed_origins`: the app addresses a scan may test. Without one, only the
  operator can let a scan probe a running app, and even then only from this
  list.
- `max_chunk_lines` (default 4000): how much one reviewer reads at once.

## Use it

Ask for a review: "scan this repo for security issues", or "and run tests to
confirm" to allow reproductions. The agent gets nine tools in the `security`
toolset:

| Tool | What it does |
| --- | --- |
| `security_scan_start` | Walk the folder, plan chunks of about 4,000 lines, check the manifests against OSV, answer the scan id. |
| `security_scan_get` | Read the scan: `next` is the one step to take now; `summary`, `packets`, `inventory`, `coverage`, `workers`, `findings`, `chains`, `validations`, `activity`, `manifest`, `report` are the data under it, paged. |
| `security_scan_checkpoint` | Record what was really read, and an optional note, or mark the scan blocked with the reason. |
| `security_scan_submit_worker_result` | Take one reviewer's candidates with the snapshot digest it read; a payload from another snapshot is refused. |
| `security_scan_record_validations` | Record receipts and plans: `CONFIRMED` only when the test passed and a negative control behaved, else `NOT_RUN`. |
| `security_scan_record_chains` | Compose the chains the primitives in one file form, with the reviewer's words for them. |
| `security_scan_finalize` | Seal the scan into `report.md`, `findings.json`, `results.sarif` and `findings.csv`. |
| `security_scan_cancel` | Stop an unfinished scan and keep its evidence. |
| `security_scan_export` | Read one sealed report. |

The operator's command is `/security`:

```
/security status
/security authorize-validation <scanId> <origin>   let the scan test your running app
/security revoke <grantId>
/security help
```

A grant is minted only by that command, never by a tool, names one origin, and
expires after `grant_minutes` and `grant_requests`.

What a scan keeps is what the original kept: the folder it reviews, the files it
read and the ones it skipped with the reason, the chunks, the reviewers'
results, coverage, candidates with the exact path and line each was found at, the
receipts that confirm or refute them, the chains, and the sealed reports. A
finding carries a rule, a severity, its evidence state, its taxonomy (CWE,
OWASP), its validation status and a fingerprint derived from the title, the
locations and the snapshot; never a secret's value.

Zero findings with chunks nobody read is a gap, not a clean result: the report
says how many chunks were reviewed, and `security_scan_get section next` keeps
naming the step until the work is done.

The one network call is the dependency check: the packages your
`package.json`, `composer.json` or `requirements.txt` declare are sent by name
and version to `https://api.osv.dev/v1/querybatch`, and the advisories come
back into the report. No code, no path and no environment leaves the machine.
`dependency_check: false` in the section, or `dependency_check: false` on a
scan, turns it off.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The scan record, the walk, the OSV call, the four report writers. |
| `tools.lisp` | The nine tools, `/security`, and the contracts that declare them. |
| `package.lisp` | The package, its exports and the cell's condition. |
| `test/` | Offline tests: the network is stubbed, every report is read back. |

## Licence

Apache-2.0. The plugin is Copyright (c) 2026 unsupportedpastels: see `LICENSE`
and `NOTICE`.
