# nodecode-bumblebee

Bumblebee Guard: known-compromised npm and PyPI releases among the packages installed in a folder, plus OSV.dev advisory counts. A Nodecode rewrite of [kahnanX/bumblebee-guard](https://github.com/kahnanX/bumblebee-guard) by [kahnanX](https://github.com/kahnanX).

## Get it

Tell the agent "add the bumblebee cell", or run `nodecode add nc://bumblebee`.

## Set it up

No key and no account. The catalog ships with the cell; OSV.dev is public. Its section is optional:

```jsonc
"bumblebee": {
  "roots": ["~/projects"],  // folders a scan walks when a call names none
  "max_depth": 6,           // how deep below each root the walk goes
  "max_packages": 20000,    // most packages inventoried in one scan
  "osv_enabled": true,      // ask OSV.dev for advisory counts, one batch call a scan
  "osv_max_packages": 50,   // most packages OSV is asked about, one query each
  "timeout_seconds": 20     // the OSV call's ceiling
}
```

Set `"osv_enabled": false` for a machine with no network: the curated match needs none.

## Use it

A release is compromised in its own right when the package was published by someone the maintainer did not choose, and what it ran was not the library. The worst are listed in the cell's catalog by ecosystem, name and exact version, with the incident and one line to act on. A scan reads the disk -- every `package.json` under a `node_modules` folder, every `.dist-info` folder Python leaves beside an installed distribution -- and matches each `(ecosystem, name, version)` exactly. It runs no package manager and no shell:

```
/bumblebee scan ~/work/some-project
  bumblebee: 1 known-compromised package among 412 packages under ~/work/some-project
  [critical] npm/event-stream@3.3.6 -- .../node_modules/event-stream
      pulled flatmap-stream, which read a wallet library and shipped a bitcoin stealer
  osv: 7 advisories across 50 packages checked (vulnerabilities, not confirmed malware)
```

- `bumblebee_guard_scan path="FOLDER"` -- the tool the model calls; `osv=false` skips the network.
- `bumblebee_guard_status preview=true` -- the last scan without walking anything: when, how many packages, which findings, how many advisories.
- `bumblebee_guard_diff action=set_baseline|show` -- what appeared and what went away since the baseline.
- `/bumblebee [status | scan FOLDER | diff [show|set_baseline]]` -- the same for the operator.
- `(bumblebee:scan "FOLDER")`, `(bumblebee:status)`, `(bumblebee:diff "show")` -- the same through eval, and `(help :bumblebee)` for the manual.

A scan with findings stands as a notice under the cell's key, so the model reads it at its next request without anyone asking; a clean scan clears it, so a clean machine costs no context. That is the rewrite of the original's rate-limited `pre_llm_call` injection, with no hook and no rate limit: a scan is explicit, and the notice belongs to the last one.

Doctrine: a curated match is a fact about exactly these bytes, and high confidence -- investigate, do not auto-ignore. OSV counts are advisories, not confirmed malware: deduplicate aliases and check that the installed version is affected before acting. Zero findings means clean against this catalog, not intrusion-free: file contents are never scanned, and only npm and PyPI are inventoried.

## What is left out

- **The scanner bridge.** The original wrapped a separately installed bridge and ran its `daily_scan.sh`, which inventoried npm, PyPI, Go, RubyGems and Composer packages, MCP server configs and editor/browser extensions, and read the machine's whole home. This cell walks one folder you name, in Lisp, with no external program, and reads npm and PyPI only. MCP configs, editor and browser extensions, and the other ecosystems are not inventoried.
- **The full curated catalogs.** The original loaded the bridge's catalog files (Shai-Hulud variants, gemstuffer and others). This cell carries a sample of 23 rows, one entry per compromised release, written into `scan.lisp`. Reading catalog JSON from a folder is not done.
- **The rate limit.** The original injected at most one alert an hour because its hook ran on every model call. Here the alert is a standing notice that a scan sets and a clean scan clears, so there is nothing to rate-limit.
- **The confidence-class skill.** The triage doctrine is in the manual (`(help :bumblebee)`) and in this README rather than in a separately registered skill file.

Data over code: the catalog and the manual are `defparameter`s, so a layer can rebind them.

## Test

```sh
.github/test-cell.sh tools/nodecode-bumblebee
```

13 tests, no network: every tree is built in a temporary folder, and the OSV call is stubbed.
