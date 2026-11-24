# nodecode-snyk

Snyk Security Scan: scans code, dependencies, containers and IaC for vulnerabilities with the Snyk CLI. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

## Get it

Tell the agent "add the snyk cell", or run `nodecode add nc://snyk`.

## Set it up

The cell runs the Snyk CLI, which is yours; nothing here vendors Node, npm or
the original's MCP server. Install it once:

```sh
npm install --global snyk
```

Then sign in — `/snyk login` in Nodecode, or `snyk auth` in a terminal. A
browser window opens and the token is kept in the Snyk CLI's own config, never
in Nodecode, so the cell's section holds no secret. A free Snyk account covers
the code (SAST) and open-source (SCA) scanners.

Without a global install, point the cell at the same pinned release the plugin
launched:

```jsonc
{
  "snyk": {
    "command": ["npx", "-y", "snyk@1.1306.0"],
    "severity_threshold": "high"
  }
}
```

The other members, all optional:

| Member | Default | What it does |
| --- | --- | --- |
| `command` | `["snyk"]` | the argv prefix that runs the CLI |
| `timeout_seconds` | `600` | one run's ceiling; past it the whole process tree is killed |
| `max_output_chars` | `20000` | how much of what the CLI wrote one tool result carries |
| `severity_threshold` | `"low"` | the floor a scan reports when its call names none (a code scan has no critical rung and reads it as high) |

## Use it

Ask for a scan, or call the tools yourself. Each answers the command it ran,
how it ended (`exit 0` nothing found, `1` issues found, `2` the scan failed,
`3` no supported project, `127` the CLI is not installed), then the CLI's own
JSON, cut at `max_output_chars`.

| Tool | What it scans |
| --- | --- |
| `snyk_code_scan` | source, without running it (Snyk Code, SAST) |
| `snyk_sca_scan` | open-source dependencies, from the project's manifests |
| `snyk_container_scan` | a container image's OS packages and app dependencies |
| `snyk_iac_scan` | Terraform, Kubernetes, CloudFormation, ARM, Serverless |
| `snyk_sbom_scan` | an existing CycloneDX or SPDX SBOM |
| `snyk_aibom` | a Python project's AI bill of materials (preview channel) |
| `snyk_auth`, `snyk_trust`, `snyk_logout`, `snyk_version` | the account, one trusted folder, the session, the CLI's version |

Every `path` is absolute (the original's skill says why: each scanner resolves
a relative one against its own working directory), and the cell refuses one
that is not. The argument names are the original MCP server's, so
`severity_threshold`, `all_projects`, `command`, `print_deps`,
`exclude_base_image_vulns`, `scan` and the rest read as its documentation
describes them; each tool's line in the declaration says what it takes.

`/snyk` reports the CLI's version and the account it is signed in to;
`/snyk login` and `/snyk logout` do what they say. `(help :snyk)` carries the
workflow the plugin's skill taught: sign in, scan the folder you were asked
about, read the findings as file:line with the rule and the fix, fix what was
agreed to, re-scan, and report the before and after counts.

What a scan sends: SAST uploads source and SCA uploads the dependency graph to
Snyk's cloud. Ask before scanning a private or client repository.

## What the rewrite does not do

- `snyk_package_health_check` and `snyk_breakability_check`: the MCP server
  answered them from Snyk's package-intelligence API, and the CLI has no
  subcommand for either.
- `snyk_send_feedback`: outbound issue-count telemetry, which the original
  marks opt-in.
- Six rarely used SCA flags are not carried: `assets_project_name`,
  `configuration_matching`, `gradle_sub_project`, `fail_fast`, and the .NET
  pair `dotnet_runtime_resolution` / `dotnet_target_framework`.

## Test

```sh
.github/test-cell.sh tools/nodecode-snyk
```

Fourteen tests, offline: every Snyk run goes through one stubbed seam, so the
command line a tool builds and the string it answers are asserted with no CLI
installed, no account and no network.

## License

MIT, Nous Research's: see `LICENSE` and `NOTICE`.
