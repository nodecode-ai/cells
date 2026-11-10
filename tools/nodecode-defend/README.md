# nodecode-defend

Defend: scans text for prompt-injection, jailbreak and leaked-secret patterns, and can stop a tool call that carries one. A Nodecode rewrite of [PerryLink/dsh-defend](https://github.com/PerryLink/dsh-defend) by [PerryLink](https://github.com/PerryLink).

## Get it

Tell the agent "add the defend cell", or run `nodecode add nc://defend`.

## Use it

Every tool call is read before it runs: the form the agent wrote and the
strings it carries. If a rule matches, the call is allowed, put up as a card,
or refused, by family:

- **injection** -- "ignore all previous instructions", a fake system message,
  a request to print the system prompt.
- **jailbreak** -- "act as a DAN", developer mode, "no rules apply".
- **secret** -- an API key, a token, a private-key block typed into a call.

With `ask` (the default for all three), the web page and the desktop app put
up a card above the message box: the rule, the call, and **Allow once** or
**Deny**. Allow once runs that one call; Deny refuses it and tells the agent
you chose Deny. With nobody to ask -- only a terminal or a channel lane -- the
call is refused, saying so. `block` refuses at once; `allow` lets the family
through and still records the match.

A critical secret (a live OpenAI, Anthropic, GitHub or AWS key, a Slack token,
a private-key block) is blocked whatever `secret_action` says, until you turn
`secret_block_critical` off. A secret candidate is only admitted when its
entropy clears `min_secret_entropy`, so `password: aaaaaaaaaaaa` is not one.

Nothing repeats the matched text: a secret is reported by its type only, and
the report keeps a rule id, a family and a decision.

### The verbs

- `defend_scan` -- scan any text and answer its matches: use it on a page, an
  issue or a message you did not write yourself.
- `defend_report` -- the counts since the cell started, as JSON.
- `/defend` -- the same report as a line, with the settings it was decided by.

From eval: `(defend:scan-text TEXT)` answers the matches,
`(defend:report-text)` what has been seen, and `(help :defend)` prints the
short manual.

## Set it up

No key, no account. The defaults are the ones above; change them in the
`defend` section of `~/.nodecode/config.jsonc` or in `/settings`:

| Key | Default | What it does |
| --- | --- | --- |
| `scan_calls` | `true` | scan every tool call before it runs; `false` leaves the verbs and `/defend` |
| `injection_action` | `ask` | `allow`, `ask` or `block` an injection match |
| `jailbreak_action` | `ask` | the same for a jailbreak match |
| `secret_action` | `ask` | the same for a leaked credential |
| `secret_block_critical` | `true` | block a critical secret whatever `secret_action` says |
| `min_secret_entropy` | `3` | bits per character a secret candidate must clear; `0` admits every match |
| `max_scan_chars` | `10000` | only the head of a longer text is scanned |
| `report_entries` | `200` | how many recent matches the report keeps |

This is a speed bump, not a security boundary: eval has the machine's whole
authority, and a payload written to look like ordinary prose can pass a
pattern. It catches the common slips.

## Testing

- From a Nodecode checkout, `just defend-test`.
- From this repository, `.github/test-cell.sh tools/nodecode-defend`, offline
  against a pinned Nodecode tree.

The tests cover the scanner against each family (a payload found, a clean text
passed, a low-entropy secret dropped, the head-only cap), the tier each
family decides, that a critical secret overrides `secret_action`, that a call
nothing matches runs, that a matched one is refused with nobody to ask and
runs on Allow once, that the scan and report verbs and the `/defend` command
answer, and that no matched text is ever quoted back.
