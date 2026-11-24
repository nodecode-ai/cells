# nodecode-doctorpack

Doctorpack: local diagnostics, a safety audit and redacted incident postmortems for this machine's Nodecode home. A Nodecode rewrite of [godhiraj-code/hermes-doctorpack](https://github.com/godhiraj-code/hermes-doctorpack) by [godhiraj-code](https://github.com/godhiraj-code).

It reads the home it runs in -- `config.jsonc`, the `cells/` folder, and the
home's own log files -- and answers findings, not guesses: every one carries a
status (`ok`, `warn`, `fail` or `unknown`), a severity, a confidence and the
evidence it was read from. `unknown` means the audit lacked evidence; a home
with no log file is not a broken home. Nothing leaves the machine, and
nothing calls the network.

## Get it

Tell the agent "add the doctorpack cell", or run

```
nodecode add nc://nodecode-doctorpack
```

## Set it up

Nothing is required. With no `doctorpack` section at all it audits the home it
runs in. To point it elsewhere, or to bound what it reads:

```jsonc
{
  "doctorpack": {
    "home": "~/.nodecode",      // another profile's home to read; unset = the active one
    "lookback_hours": 24,        // how far back a log line counts
    "max_evidence": 20,          // how many evidence snippets a postmortem keeps
    "include_logs": true         // false keeps the log scan out of the config audit
  }
}
```

## Use it

The model gets three tools:

| Tool | What it answers |
| --- | --- |
| `doctorpack_audit` | the config audit: is `config.jsonc` there, which sections it names, whether a credential is pasted into it inline, and the recent warning/error lines of the home's logs |
| `doctorpack_cells` | the `cells/` folder: which folders are there, which carry no `.asd`, and the recent cell-related log lines; pass a folder name to answer for one |
| `doctorpack_postmortem` | one redacted markdown report under `<home>/doctorpack/reports/`, its filename and the findings it holds |

The operator gets the command (`(help :doctorpack)` is the same text):

```
/doctorpack [scan | cells | postmortem] [--lookback-hours N] [--max-evidence N]
            [--incident-title TEXT] [--cell NAME] [--home PATH] [--json]
```

Typed alone it prints the audit as lines: the overall status, the counts, and
each finding with its fix. `--json` prints the whole payload instead.

Everything returned has been through one redaction pass first: assigned keys
and tokens, bearer credentials, issuer-prefixed token shapes, credentials in a
URL's userinfo, email addresses and phone shapes are masked, and a
credential-named config member reads `[REDACTED]` whatever it holds. The
report masks the home's own path too, so a postmortem can be pasted into a
chat.

## What it does not do

It does not read Hermes, another harness's home, or any other application's
files: the settings name one home and the audit reads that one. It does not
repair anything -- a finding says what to change, not does it.

## Credit

The checks, the finding model, the redaction rules and the report layout are
[godhiraj-code](https://github.com/godhiraj-code)'s, from
[hermes-doctorpack](https://github.com/godhiraj-code/hermes-doctorpack) at
commit `ac00ace14ea9210e2e02a7b978cf4bc51eec2d8f`, MIT. See `NOTICE` for what
was rewritten and what was left out.
