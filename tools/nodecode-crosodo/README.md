# nodecode-crosodo

Crosodo: US cottage-food and home-bakery law by state and county. A Nodecode rewrite of [Terahertz-Inc/crosodo-mcp](https://github.com/Terahertz-Inc/crosodo-mcp) by [Terahertz-Inc](https://github.com/Terahertz-Inc).

## Get it

Tell the agent "add the crosodo cell", or run `nodecode add nc://crosodo`.

## Set it up

Nothing is required. The endpoint answers its lookups without a key, and the
cell talks to it at `https://crosodo.com/api/mcp`. A section is only for
moving those defaults:

```jsonc
{ "crosodo": { "base_url": "https://crosodo.com/api/mcp", "timeout_seconds": 40, "answer_chars": 12000 } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `base_url` | `https://crosodo.com/api/mcp` | the MCP endpoint; it must carry `https://` or `http://` |
| `timeout_seconds` | `40` | seconds one lookup or checkout may take |
| `answer_chars` | `12000` | the longest answer the model reads from one call |

A `base_url` with no scheme refuses the start and says so, and then no tool
is offered; `(restart-cells "nodecode-crosodo")` tries again after the fix.

## Use it

Ask a cottage-food question: whether cookies may be sold from a home kitchen
in Virginia and under which sales cap, what Fairfax County requires, how two
states differ, or whether a Crosodo Certified credential stands. The model
calls one of six tools, each answered over JSON-RPC by the endpoint:

| Tool | What it answers |
| --- | --- |
| `lookup_state_law` | one state's rules: sales cap, registration, allowed products, statute and agency links |
| `lookup_county` | a county's requirements: contacts, verification status, local links |
| `list_products` | the catalog: the free state-law report and what a checkout can sell, with prices |
| `start_checkout` | a Stripe Checkout url for one product; it never charges |
| `verify_certification` | a Crosodo Certified credential's public fields |
| `crosodo_call` | any tool the endpoint lists that the five above do not cover, by its own name |

The same calls from eval:

```lisp
(crosodo:lookup-state-law "VA")                       ; a state's rules
(crosodo:lookup-county "VA" "Fairfax")                ; a county's, under its state's law
(crosodo:products)                                    ; what a checkout can sell
(crosodo:start-checkout "permit_guide" :state "VA" :county "Fairfax" :idempotency-key "k-1")
(crosodo:verify-certification "CC-VA-26-1042")        ; format example, not a real credential
(crosodo:call-tool "list_products")                   ; the escape hatch
```

`/crosodo` on its own opens a panel: a row a lookup, each running the command
with what you type after it. `/crosodo CA` reads a state's law,
`/crosodo county VA Fairfax` a county's, `/crosodo products` the catalog,
`/crosodo verify CC-VA-26-1042` a credential, and
`/crosodo buy permit_guide VA Fairfax` a checkout.

`(help :crosodo)` answers the same guidance the model reads; `*primer*` is
the text, so a layer can reword it.

What the cell asks of the model: name the state, and the county when one was
asked, because the law differs by place; never invent statute text; say
plainly that this is legal information and not legal advice; and start a
checkout only when the user clearly asks to buy. An answer the endpoint marks
a failure arrives as `ERROR:`, in the endpoint's own words.

Delivery is JSON-RPC over one POST a message, on the image's own HTTP call:
the handshake goes out once per start, and a session the endpoint has
forgotten costs one more, not a round.

Run `.github/test-cell.sh tools/nodecode-crosodo` from the cells repository.
