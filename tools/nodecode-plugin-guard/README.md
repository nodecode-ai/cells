# nodecode-plugin-guard

Plugin Guard: review a plugin or cell folder before you trust it, with the folder read as text and never run. A Nodecode rewrite of [mauricemohr88-debug/hermes-plugin-guard](https://github.com/mauricemohr88-debug/hermes-plugin-guard) by [mauricemohr88-debug](https://github.com/mauricemohr88-debug).

## Get it

Tell the agent "add the plugin guard cell", or run `nodecode add nc://plugin-guard`.

## Use it

Ask the agent to review a folder before you install it, or type it yourself:

```
/plugin-guard path/to/some-plugin
/plugin-guard rules
```

The model has `plugin_guard_scan` for the same review: give it a folder, absolute or relative to the project.

A report is six or seven lines and then one line a finding:

```
plugin-guard: some-plugin -- 42 files read, 1 unread, 3 findings: 1 critical, 2 high
FAIL at high: 3 findings at or above it
  CRITICAL  PG201 some-plugin/auth.json:1 - auth.json usually holds credentials
  HIGH      PG103 some-plugin/run.py:12 - process execution: subprocess.
  HIGH      PG104 some-plugin/run.py:30 - sensitive path reference: /.ssh/
read as text, never run: a review aid, not a verdict.
```

Passing is not permission to trust the code: it means a first pass found nothing loud. Read what it points at.

## What it looks at

Nothing in the folder is imported, compiled or run: every file is read as bytes, then matched as text. The walk is bounded (`max_files`), and so is the report (`max_findings`).

| Rule | Severity | Finding |
| --- | --- | --- |
| PG001 | high | no plugin or cell declaration (`plugin.yaml`, `pyproject.toml`, `package.json`, `*.asd`) |
| PG002 | medium | no LICENSE file |
| PG003 | low | no SECURITY.md |
| PG004 | low | no automated tests |
| PG101 | high | dynamic code execution: `eval(`, `exec(`, `__import__(`, `compile(` |
| PG102 | high | unsafe deserialization: `pickle.load`, `marshal.load`, `yaml.load(` and its kin |
| PG103 | high | process execution: `subprocess.`, `os.system(`, `run-program` |
| PG104 | high | a sensitive path: `/.ssh/`, `id_rsa`, `/.aws/`, `/.netrc`, cookies, wallets |
| PG105 | high | TLS verification disabled |
| PG106 | info | a network-capable module imported |
| PG107 | medium | an environment read that names a credential |
| PG108 | medium | a destructive filesystem call |
| PG110 | high | a privileged registration surface (tool override, pre-auth hook, middleware, message injection) |
| PG112 | medium | a concrete outbound request |
| PG201 | critical | a credential-looking filename or literal (private key, GitHub/Slack/AWS token, `sk-` key) |
| PG202 | high | a remote dependency with no 40-character commit id |
| PG203 | low | a dependency with no upper bound |
| PG204 | high | a downloaded script piped to a shell |

A rule fires once per file, at the first line that carries it: this is a review aid built on text, not a parser, and it favours a line you can go and read over a proof.

## Set it up

Nothing is required. The section `plugin-guard` in `~/.nodecode/config.jsonc` may carry:

- `min_severity` — the line the report's `FAIL` or `ok` judges at; `high` by default.
- `exclude` — rule ids to leave out of every report, e.g. `["PG203"]`.
- `max_findings` — the longest list one report prints; 20 by default.
- `max_files` — how many files one review reads; 500 by default.

Leave rule ids out for a whole class of folder — a vendored tree, a test fixture — rather than reading the same finding every time.

## What is left out

The original's Hermes-only surfaces: the subprocess worker with resource limits, the opaque location-id projection (a review here already reports in-tree names only), the JSON, SARIF and GitHub-actions reporters with their exit codes, and the Hermes manifest, entry-point, hook-drift and activation-state checks. `NOTICE` says why, one by one.

## Test it

```sh
.github/test-cell.sh tools/nodecode-plugin-guard
```
