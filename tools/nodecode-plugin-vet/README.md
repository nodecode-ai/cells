# nodecode-plugin-vet

Plugin Vet: a five-dimension supply-chain risk card for a plugin, a cell, an npm package or a folder, read-only. A Nodecode rewrite of [PerryLink/dsh-skill-pack-security](https://github.com/PerryLink/dsh-skill-pack-security) by [PerryLink](https://github.com/PerryLink).

## Get it

Tell the agent "add the plugin-vet cell", or run `nodecode add nc://plugin-vet`.

## Set it up

Nothing is required. The section `plugin-vet` in `~/.nodecode/config.jsonc` takes:

- `token_env`: the name of an environment variable holding a GitHub token, which lifts the anonymous API rate limit and changes nothing else.
- `max_files` (400) and `max_file_bytes` (262144): how much of a local folder one scan reads.
- `timeout_seconds` (15): one request's ceiling.
- `gate` (`warn` or `deny`): whether a FAIL is only reported or also held against the install.

## Use it

Ask "is `owner/repo` safe to install?" and the model calls `plugin_vet`; or run `/plugin-vet`:

```
/plugin-vet PerryLink/dsh-skill-pack-security
/plugin-vet npm:left-pad@1.3.0
/plugin-vet ~/projects/some-plugin
```

The answer is one card: a verdict (PASS, WARN or FAIL) and a score for licence, source, dependencies, build scripts and maintenance, then one line per finding with the file and line it came from and the audit skill section that continues the review by hand. `(help :plugin-vet)` has the method and the nine checks.

What it reads is data, never code: `package.json`, the licence file, lockfiles, workflow files, scripts and shipped prose. Nothing is written, nothing is executed. A GitHub target costs three GETs (the repository, its `package.json`, its licence); an npm target one; a local folder none.

The nine checks: licence and SPDX id; the dependency tree and floating versions; commit pinning of git and workflow references; lifecycle scripts that download and run, decode a payload, or touch credentials and shell profiles; receiver hosts, raw IPs and free TLDs; encoded blobs beside a decoder; archived or forked sources; maintenance age; and shipped prose that overrides or hides. Each finding names one of the pack's eight skills: `security-audit`, `dependency-audit`, `supply-chain-review`, `prompt-injection-review`, `threat-model`, `secret-scan`, `vuln-intel`, `incident-response`.
