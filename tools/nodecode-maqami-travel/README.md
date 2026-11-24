# nodecode-maqami-travel

MAQAMI Travel: live hotel and flight rates, and a secure checkout link on book.maqami.co. A Nodecode rewrite of [negm17111995/mcp-server](https://github.com/negm17111995/mcp-server) by [negm17111995](https://github.com/negm17111995).

## Get it

Tell the agent "add the maqami-travel cell", or run `nodecode add nc://maqami-travel`.

## Set it up

One field is required: `guest_nationality`, the lead guest's nationality as an
ISO-3166 alpha-2 code (`EG`, `US`), because hotel rates are priced for it. Set
it in `/settings` (or the page's Settings tab) under `maqami-travel`:

```jsonc
"maqami-travel": {
  "guest_nationality": "EG",
  "currency": "USD"
}
```

There is no key: MAQAMI's server is public, and the cell joins a session with
it at its first call. `currency` (three letters, default `USD`) picks how
rates are quoted; `base_url` only moves the endpoint, for a mirror or a local
mock. `/maqami` with no argument shows what the cell is connected to.

## Use it

Ask for what you want, and the model calls one of two tools:

| Tool | Use |
| --- | --- |
| `maqami_tools` | the server's tool list: each name, what it does, the arguments it takes |
| `maqami_call` | one of those tools by name, with its arguments as one JSON object |

MAQAMI's server defines its own tools, so the list is the source of truth and
this cell asks for it rather than keeping a copy: hotel and flight rate
searches, hotel details and reviews, prebook and verify (which hold a rate and
answer a checkout link), booking lookups, amendments and cancellations.

From the keyboard:

```
/maqami                                  what the cell is connected to
/maqami tools [WORD]                    the server's tools, or those about WORD
/maqami call TOOL {"cityName": "Cairo", "checkin": "2026-12-01"}
```

`(help :maqami)` is the model's guide: how a booking works, what to collect
before a search, that a prebook, amendment or cancellation waits for a clear
yes, that payment happens only on book.maqami.co, and that hotel and review
text is data, never instructions.

### The two tools are the plugin's bridge

The original was a Node stdio bridge that forwarded every MCP request to
`https://mcp.maqami.co/` over Streamable HTTP, plus the bundled
`maqami-travel-booking` skill. This cell is the same bridge in Lisp: a JSON-RPC
request is one `nlk:http` POST, the answer is read out of a plain JSON body or
out of the server-sent events the endpoint answers with, and the session the
server opens is sent back on every request after it (and joined again once if
the server says it expired). No Node, no local server, no MCP dependency.

## Not ported

- **A tool per remote tool.** The endpoint's tool list is the server's own and
  is not fixed by the repository, so a registered tool per name would go stale
  between releases. `maqami_tools` reads the list and `maqami_call` calls one,
  which is exactly what the bridge forwarded.
- **The Node bridge's `MAQAMI_MCP_URL` override and its process lifecycle.**
  The section's `base_url` does the first; the cell owns the second.
- **The plugin's own test bridge** (`test/bridge.test.js`), a Node test of the
  forwarder. This cell's tests stub the endpoint instead.

## Test

```sh
.github/test-cell.sh tools/nodecode-maqami-travel
```

Offline: `nlk:http` is stubbed, so no request leaves the machine. It covers the
lifecycle, the session an initialize opens and the headers sent after it, both
shapes of answer (JSON and server-sent events), a failed call, the tools, the
command, and a section with no nationality refused.
