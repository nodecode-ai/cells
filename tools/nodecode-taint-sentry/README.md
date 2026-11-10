# nodecode-taint-sentry

Taint Sentry: stamps every tool result with where it came from, then gates the
dangerous half of a session that has read something a stranger wrote. A Nodecode
rewrite of [YAN-XBT/hermes-sentry](https://github.com/YAN-XBT/hermes-sentry) by
[YAN-XBT](https://github.com/YAN-XBT).

A page an agent fetched is not allowed to give it orders. The original puts a
wall around the tool path: every result is stamped with its author, content
written by a third party is wrapped in untrusted markers and marks the session
tainted, and while the taint is live a policy judges each call. This cell is
the same wall at Nodecode's one interception point, the `:TOOL` hook, which
sees both the call and its result.

- **Provenance, not content.** A web fetch, a search, a browser, an MCP server,
  a screenshot, an image, or a file this session itself wrote while its content
  was untrusted marks the session. A novel injection phrasing still gets gated,
  because the turn that read the page is gated, whatever the page said. A plain
  local file read is the user's own material and does not mark anything.
- **Stamp.** A marked result loses its invisible characters (zero-width, bidi
  overrides, tag characters), has its instruction-shaped lines named
  (`instruction_override`, `secret_target`, `egress_instruction`, ...), and is
  wrapped in `<untrusted source="..." reason="..." id="...">` markers with a
  note that the block is data. A trusted result is not touched at all.
- **Gate.** While the taint is live: reading a credential, writing a file the
  agent reads as instructions later, and running a file the session downloaded
  are refused; sending data to a host nobody allowlisted and writing outside the
  workspace are held for the operator. A rule that does not depend on taint --
  a credential read and a send in one shell step, or an edit to this cell's own
  policy -- applies always. An untainted session is never gated.

This is a seatbelt, not a sandbox. The taint mark is reliable, because it comes
from which tool produced the content; the credential, egress and write
detectors are pattern matching over a call's arguments, so an obfuscated
command, a payload assembled to avoid the patterns, and a write through a tool
this cell does not read can get past them.

## Get it

Tell the agent "add the taint-sentry cell", or run `nodecode add nc://taint-sentry`.

## Set it up

No key, no service, no network: the cell uses cl-ppcre over the calls it sees
and keeps its state in memory. To change what it does, edit the `taint-sentry`
section of the shared config (or `/settings`):

| Member | Default | What it does |
| --- | --- | --- |
| `mode` | `enforce` | `enforce` gates a tainted session, `monitor` records only, `off` does nothing |
| `taint_ttl_seconds` | `1800` | how long a mark stays live |
| `allow_hosts` | `localhost`, `127.0.0.1`, `::1`, `0.0.0.0` | hosts a tainted session may send data to |
| `allow_paths` | none | paths the policy never gates |
| `trusted_tools` | none | tools whose result is never treated as untrusted |
| `untrusted_tools` | none | tools always treated as untrusted |
| `disabled_rules` | none | rule names to turn down to `allow` |

```jsonc
"taint-sentry": {
  "mode": "enforce",
  "taint_ttl_seconds": 1800,
  "allow_hosts": ["localhost", "127.0.0.1"]
}
```

## Use it

It works without being asked: fetch a page, and the rest of the turn is gated.

```
taint_status   {tool, command}   # what this session carries, and how a call is judged
taint_receipts {last}            # what was stamped, gated, held and cleared (never the text)
/taint-sentry                    # the same, from the prompt
/taint-sentry receipts 20
/taint-sentry policy             # the rules as they stand
/taint-sentry clear              # drop every mark
(help :taint-sentry)             # the manual, in one paragraph
```

`/taint-sentry clear` is the operator's escape hatch: the taint is held in
process memory, so clearing it is the one way back to an ungated session
without a relaunch.

## Gaps

The original writes a receipts ledger to disk (`receipts.db`) and has a CLI with
a dry-run policy tester, a desktop pane and a dashboard route. This cell keeps
its receipts in a ring of the session's newest 200, answers the same questions
through `taint_receipts` and `/taint-sentry`, and has no durable ledger and no
pane. Its `pre_llm_call` banner has no point to ride here: the envelope carries
the same words into the conversation, and `(help :taint-sentry)` into every
request's manual. The obfuscated-payload decoding (`powershell -enc`) is not
carried.

## Testing

From the cells repository: `.github/test-cell.sh tools/nodecode-taint-sentry`.
The suite runs offline and needs nothing installed.
