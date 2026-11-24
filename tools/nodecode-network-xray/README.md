# nodecode-network-xray

Network X-ray: one URL's DNS, TCP, verified TLS and HTTP HEAD evidence, from this machine, saved and compared. A Nodecode rewrite of [rusty4444/hermes-network-xray](https://github.com/rusty4444/hermes-network-xray) by [rusty4444](https://github.com/rusty4444).

"Works here, not there" is the question this answers. One call walks a URL
from the machine Nodecode runs on and keeps what each stage said: the
addresses DNS answered, the one address that was connected, whether the
certificate verified and who issued it, and the status line of a HEAD.
Another machine's answer -- imported as the bundle its collector wrote --
sits beside it, and the comparison says which stage diverged, what that
difference does and does not prove, and one follow-up worth asking for.

The probe is deliberately small: one direct connection, so proxy settings,
browser cookies and browser trust stores do not apply; no query string, no
credential, no redirect followed, no response body, one address per run, and
the certificate verified. Private and loopback targets are allowed, because
that is usually where the difference is -- it is not a scanner.

## Get it

Tell the agent "add the network-xray cell", or run `nodecode add nc://network-xray`.

## Set it up

One setting is required: the **vantage label**, which says which machine a
probe speaks for, so two records from two machines are never confused for
one. Put it in the cell's `network-xray` section (under `/settings` or the
Settings tab), or leave it out and pass `label` on the call:

| Setting | Default | What it does |
| --- | --- | --- |
| `vantage` | — (required) | how a probe from this machine is labelled, e.g. "laptop on home wifi" |
| `timeout` | 3 | seconds one stage may take before the capture gives up on it |
| `max_records` | 50 | the most records one history answer lists |
| `directory` | empty | where the records live; empty means the cell's own folder in this profile |

Nothing else is needed: network access is the point, and the TLS trust store
is the system's.

## Use it

Ask in words -- "why does https://example.com reach my laptop but not the
server", "what does DNS answer for this host from here" -- and the model
calls `network_xray_probe`. Then `network_xray_history` lists what is
saved, and `network_xray_compare` reads two of them side by side. Or call
them from eval:

```lisp
(network-xray:probe "https://example.com/" :label "laptop at home")
(network-xray:history 10)
(network-xray:compare-records-by-id "left-id" "right-id")
```

`/network-xray https://example.com/` prints the same ladder as lines,
`/network-xray history` the records this profile holds, and
`/network-xray compare LEFT RIGHT` the findings. `(help :network-xray)` is
the manual.

### The second vantage

A probe runs where Nodecode runs, never on the browser or the phone talking
to it. For the other machine, take the probe there -- another Nodecode with
this cell answers the same URL -- and import the record it wrote:

```lisp
(network-xray:import-bundle (uiop:read-file-string "peer.json"))
```

An imported bundle is a recorded, unauthenticated observation: its digest
shows the content did not change in transit, never who produced it. The
original plugin's portable collector writes bundles of the same shape, so its
file imports here unchanged.

### Reading a comparison

`verdict` is one of `both_healthy`, `both_failed`, `diverged` or
`inconclusive`; `first_divergence` names the earliest stage that differs.
The findings keep three kinds apart: `observation` (what the two records
say), `hypothesis` (what it might mean, worded as a guess) and `limitation`
(what the two captures cannot show). `next_tests` carries at most one
suggestion, a `connect_ip` experiment that dials the other machine's address
with the original Host and TLS server name and full certificate
verification -- and it asks before it runs. A different DNS answer or a
different certificate is ordinary CDN behaviour, not proof of a cause.

Keep private evidence private: a record carries the URLs and the addresses
of the machine that took it.

## Tests

```
NODECODE_SRC=<nodecode checkout> .github/test-cell.sh tools/nodecode-network-xray
```

The suite is offline: the four stage functions are stubbed, so the ladder,
the address selection, the canonical URL, the sealing, the store, the
import and the comparison are the cell's own code. `just network-xray-test`
in the cells repository runs the same slice.
