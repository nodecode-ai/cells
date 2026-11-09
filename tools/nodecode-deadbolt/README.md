# nodecode-deadbolt

DeadBolt: review and approve an exact new text file before the agent writes it. A Nodecode rewrite of [seanebones-lang/deadbolt-hermes-plugin](https://github.com/seanebones-lang/deadbolt-hermes-plugin) by [seanebones-lang](https://github.com/seanebones-lang).

An agent that writes a report, a generated config or any other text artifact
usually writes it and tells you afterwards. With this cell the agent proposes
the exact destination and the exact UTF-8 text first, a person reads it in a
terminal they own, and only then is the file created. Two tools carry it:

- `deadbolt_prepare_write` saves a private review request — workspace, path,
  the exact content, a nonce and a deadline — and creates nothing.
- `deadbolt_commit_write` asks a separately running DeadBolt 2.0.0 sidecar for
  exact admission of that request, and only on an `allow` creates the one new
  file. A denial writes nothing. The grant binds the run, the tool, the
  workspace, the path, the content, the nonce and the deadline, and it is
  spent once.

The cell is the same two tools, the same envelope, the same loopback call and
the same files as the plugin, reimplemented in Common Lisp against Nodecode's
cell contract. What a person does outside the agent is unchanged: the request
file is plain JSON, and `deadbolt action inspect --file …` reads it.

## Get it

Tell the agent "add the deadbolt cell", or run `nodecode add nc://deadbolt`.

## Set it up

This cell is a gate in front of a service, so the service comes first. Install
the DeadBolt engine (Apache-2.0, a separate project) and provision one finite
run with a policy for this tool id and a scoped admission credential:

```sh
deadbolt ensure --agent report-001 --lease-ttl-secs 3600
deadbolt policy --agent report-001 --tools hermes_artifact_write_v1
deadbolt action require --agent report-001 --tool hermes_artifact_write_v1
deadbolt credential issue --agent report-001 --id report-admission-001 \
  --ttl-secs 3600 --out /absolute/private/path/new-admission-credential
deadbolt serve --bind 127.0.0.1:9782 --lease-ttl-secs 3600
```

The tool id stays `hermes_artifact_write_v1` because that is the name the
engine's policy is written against; a policy already made for the plugin
admits this cell's actions unchanged. The operator token stays in the sidecar:
the cell never sees it, and never wants it.

Then give the cell its section, in `~/.nodecode/config.jsonc`:

```jsonc
{
  "deadbolt": {
    "workspace": "/absolute/path/to/reviewed-output",
    "run_id": "report-001",
    "sidecar_port": 9782,
    "admission_token_env": "DEADBOLT_ADMISSION"
  }
}
```

with the scoped credential in the environment Nodecode runs in, or pasted into
`admission_token` instead (exactly one of the two). `workspace` is an existing
absolute directory; `review_dir` is optional and defaults to
`deadbolt/requests` under the nodecode home. `/deadbolt` says where the three
stand and how many requests are waiting, and `(restart-cells "nodecode-deadbolt")`
tries again after a fix.

## Use it

The model calls the two tools; a person works the requests. Typed alone,
`/deadbolt` reports the workspace, the run, the port, whether a credential is
set and what waits; `/deadbolt pending` lists the requests still in the review
directory. From `eval` the same verbs are ordinary functions:

```lisp
(deadbolt:prepare "report.md" "A fictional reviewed report.")
;; => ("prepared": true, "request_id": "9f2c…", "review_file": "…/9f2c….json",
;;     "expires_at": 1794000000, "output_created": false, "next_action": …)
(deadbolt:commit "9f2c…")
;; => ("admitted": true, "completed": true, "result": ("path": "report.md", "bytes": 28))
(deadbolt:pending)   ; what is still waiting, newest first
```

In the operator's terminal, between the two:

```sh
deadbolt action inspect --file /the/exact/returned/review-file.json
deadbolt action approve --file /the/exact/returned/review-file.json \
  --fingerprint sha256:THE_DIGEST_YOU_ACTUALLY_REVIEWED
```

## What it does and does not protect

Only this cell's own commit is gated. Nodecode's file tools, the shell, the
browser, MCP servers and every other cell go around it, so keep Nodecode's own
approvals on; this is a reviewed-artifact route, not whole-agent containment.
It creates new files only — never an overwrite, a patch or a delete — the
parent directory must exist, one request envelope is at most 32768 bytes by
default, and the walk refuses `..`, an absolute path, a parent that leaves the
workspace once its symbolic links are followed, and an existing file.

A prepared request holds the raw proposed text: it is written 0600 in a 0700
directory, it is not encrypted, and it stays after the commit for the operator
to read or remove. The only outbound connection is HTTP to the literal
`127.0.0.1` at the configured port, endpoint `/admit-action`, carrying the
envelope and the one scoped credential; there is no redirect, no remote
service and no telemetry. Admission is not completion: a write that fails after
admission answers `admitted: true, completed: false, outcome: "unknown"`, and
the destination is then the operator's to inspect before anything is retried.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-deadbolt
```

Every exchange with the sidecar is stubbed, so the slice runs offline.

Apache-2.0. See [NOTICE](NOTICE) for what was rewritten from the plugin.
