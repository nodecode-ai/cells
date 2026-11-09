# nodecode-privacy-xray

Privacy X-ray: where your data actually goes - a local ledger of every model request, a config audit, a canary person and a leak map. A Nodecode rewrite of [MadeByBrain/hermes-extensions](https://github.com/MadeByBrain/hermes-extensions) by [MadeByBrain](https://github.com/MadeByBrain).

You type a client's name into a session. It goes to your main model, as expected.
It may also go to the vendor that names the session, to the one that writes the
recaps, to the search engine behind a web lookup, and into this home's own
stores. None of that is visible in the chat. This cell makes it visible, on this
machine only: it dials nothing at all.

## Get it

Tell the agent "add the privacy-xray cell", or run `nodecode add nc://privacy-xray`.

## Set it up

No key, no account, nothing to paste. The section is optional:

| Key | Default | What it does |
| --- | --- | --- |
| `record` | `true` | one metadata line per model request |
| `watch_terms` | none | your own names or project code names |
| `home_region` | empty | `EU,DE,CH` and the like: a vendor outside it is amber |
| `ledger_max_mb` | `5` | the ledger is rotated once at this size, to `ledger.1.tsv` |
| `scan_files` | `300` | how many files one search reads |
| `scan_kb` | `2048` | the largest file one search reads |

Set them in `/settings` or on the page's Settings tab.

## Use it

```
/xray                 what the local ledger holds: requests, destinations, canary
/xray check           the config audit, with the fix form for each finding
/xray fix aux         the fix, shown;  /xray fix aux yes  writes it
/xray canary          a fictitious person to mention in a normal message
/xray trace           requests since that canary, and the files carrying it
/xray find "a name"   a read-only search of this home's stores, counts only
/xray report          a self-contained HTML leak map
```

The model reaches the same answers through the `privacy_xray` tool, so "which
companies see my chats?" works in plain language.

**The ledger.** A hook on the round point writes one tab-separated line per
model request: time, session, turn, round, the main model, and the host the
request's credential was resolved for (a hook on the credential point). Metadata
only - never a prompt, an answer or a path. It lives beside the section's data
in `privacy-xray/ledger.tsv` under this home.

**The audit.** Read from the shared config: the main model and its host, the
auxiliary model (session names, recaps and summaries) and whether it goes
somewhere else, the failover chain, the decider model, request dumps, the
channels, web search, the usage page and MCP servers. Each finding is green,
amber or red, and the ones with a safe fix print the exact `config-set` form.
Nothing is written without an explicit `yes`.

**The canary.** A fictitious person - the `example.org` domain is reserved for
documentation and the `+44 7700 900xxx` range for fiction. Mention it in a
message, then `trace` counts the requests since it was made and searches this
home's stores for its own values.

Not legal advice and not a compliance certificate: it shows where data goes,
not what a vendor does with it afterwards.
