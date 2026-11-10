# nodecode-vibeleak

VibeLeak: an owner-approved website assessment and a read-only local PHP/source review. A Nodecode rewrite of [abundantbeing/vibeleak-atlas](https://github.com/abundantbeing/vibeleak-atlas) by [abundantbeing](https://github.com/abundantbeing).

Two workflows, one folder.

**A local review, and never the network.** `vibeleak_review` walks one folder
and answers review candidates, each with the file and line it came from: PHP
request input reaching `eval()`, `unserialize()` or a command function; a
session serializer or `display_errors` setting; session upload progress
enabled in a local config; a provider key shape in readable source; an `.env`
ignore policy worth a look. Sensitive files (`.env`, credentials, private
keys) are skipped by name and never opened, raw source is never quoted back,
and a coverage block counts every entry not read under a reason. A candidate
is a review candidate: not a confirmed vulnerability and not a security
guarantee.

**An owner-approved assessment of a website you own.** `vibeleak_scan` sends
one hostname to VibeLeak under exactly one scan unit and answers the grade,
the findings worst first and the coverage; `vibeleak_report` reads a saved
scan back by its id, optionally with its markdown report; `vibeleak_compare`
reads two saved scans of one host and answers what persisted, what is newly
observed and what was not observed again. The comparison is an observation,
not proof that a fix worked.

No state is kept: the cell writes nothing to disk, runs nothing, and makes no
request until a tool call asks for one.

## Get it

Tell the agent "add the vibeleak cell", or run `nodecode add nc://vibeleak`.

## Set it up

Nothing is needed for the local review. Set the folder it reads in
`/settings` (or the `vibeleak` section of `~/.nodecode/config.jsonc`):

| Setting | Default | What it does |
| --- | --- | --- |
| `root` | — | the folder `/vibeleak` reviews when no argument names one |
| `max_files` | `200` | how many files one review reads at most |
| `api_key` | — | a VibeLeak owner API key (Pro plan), scopes `scan:write`, `scan:read`, `findings:read`; website assessments only |
| `origin` | `https://www.vibeleak.app` | the fixed origin the assessments go to |
| `timeout_seconds` | `30` | one request's ceiling |

A key is needed only for the website half. Without one, `vibeleak_scan`,
`vibeleak_report` and `vibeleak_compare` refuse in words that name the
setting, and the local review still works.

## Use it

```
vibeleak_review                                  # the configured root
vibeleak_review path="/srv/app"                  # another folder
vibeleak_scan domain="example.com"               # one approved assessment, one unit
vibeleak_report scan_id="<uuid>" markdown=true
vibeleak_compare before="<uuid>" after="<uuid>"
```

or `/vibeleak` for the few lines an operator reads first (candidates, files
read, entries skipped), and `/vibeleak /srv/app` to review another folder.

## What it deliberately gets right

**A comment is blanked, not skipped.** The PHP rules blank comment text to
spaces and keep the newlines, so a `eval($_GET['x'])` inside a `//` comment
is not a finding and a finding's line is still the line a reviewer opens.

**A sensitive file is excluded by name, unopened.** `.env`, `.npmrc`,
`.netrc`, `credentials`, `id_rsa`, `id_ed25519`, `auth.json` and
`*.pem|p12|pfx|key` are counted as excluded, never read.

**Coverage never says clean.** Every file not read is counted under a
reason (file limit, size limit, depth, native refusal, unsupported type,
sensitive name), and a review with no finding says so without claiming
security.

**The hostname is checked before it travels.** An IP literal, a reserved
suffix, a custom port or a malformed label is refused here, not by the API.

Run `.github/test-cell.sh tools/nodecode-vibeleak` from the cells repository.
The tests build a scratch tree, stub the one network call and call every tool
through the real dispatch, so no network, clock or file of the operator's is
involved.
