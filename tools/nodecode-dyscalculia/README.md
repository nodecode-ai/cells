# nodecode-dyscalculia

Dyscalculia statistics: cited prevalence figures, references, chart data and screening-test facts from dyscalculia.io. A Nodecode rewrite of [Dyscalculia-io/dyscalculia-agent-plugin](https://github.com/Dyscalculia-io/dyscalculia-agent-plugin) by [Dyscalculia-io](https://github.com/Dyscalculia-io).

The original was configuration only: it pointed an MCP client at
`https://mcp.dyscalculia.io/mcp` and shipped one skill. Here the same endpoint
is spoken from Lisp, and its six read-only tools are six tools a model can
call:

| Tool | What it answers |
| --- | --- |
| `get_prevalence` | Published prevalence figures, one per population study or guideline: the figure or range in percent, sample size, year, stated age and the reference key. Filters: `study`, `age`, `min_pct`, `max_pct`. |
| `list_studies` | The 28 sources the statistics page cites, with DOI or PMID, link, kind and the topics each supports; filter by `topic`. |
| `get_reference` | One full reference record by key (93 keys work), with DOI or PMID and the entries built from it. |
| `list_chart_data` | The 7 chart and table datasets (persistence, maths anxiety, interventions, by-country, sex ratio, comorbidity) with their rows and CSV, JSON and SVG addresses. |
| `explain_screening_test` | The five number-skills screening games, what each measures and how they are scored; `include_items` adds the item sets. |
| `get_citation` | How to cite and reuse the compilation: the licence, the citeAs text, the credit link, the downloads. |

And `/dyscalculia` for the operator, answered from the same six.

## Get it

Tell the agent "add the dyscalculia cell", or run `nodecode add nc://dyscalculia`.

## Set it up

Nothing is required. The server needs no key, no account and no sign-in, and
the cell talks to it over the image's own HTTP call. Say `/dyscalculia` to try
it.

Three optional members, in `/settings` or in the `dyscalculia` section of
`config.jsonc`:

```jsonc
{ "dyscalculia": { "base_url": "https://mcp.dyscalculia.io/mcp", "timeout_seconds": 30, "max_chars": 8000 } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `base_url` | `https://mcp.dyscalculia.io/mcp` | the MCP endpoint; https, or http on localhost for a server of your own |
| `timeout_seconds` | `30` | how long one call may take |
| `max_chars` | `8000` | the longest answer handed to the model; longer text is cut at that many characters and says so |

A member of the wrong shape refuses the start and names itself; then
`(restart-cells "nodecode-dyscalculia")`.

## Use it

Ask the question you actually have: "how common is dyscalculia at 11?", "what
is the source for that figure?", "does it persist into adulthood?", "what does
a dyscalculia test measure?". The model picks the tool, and the answer is the
site's own text: figures with their studies, references with DOI or PMID, chart
rows with their notes.

From eval, the same six calls:

```lisp
(dyscalculia:get-prevalence :age 11)
(dyscalculia:get-prevalence :min-pct 3 :max-pct 6)
(dyscalculia:list-studies :topic "persistence")
(dyscalculia:get-reference "shalev2000")
(dyscalculia:list-chart-data :id "chart-1")
(dyscalculia:explain-screening-test :include-items t)
(dyscalculia:get-citation)
```

`(help :dyscalculia)` answers the guidance the model reads, taken from the
original's skill; `*primer*` is that text, so a layer can reword it.

What the site asks of anyone quoting it is in that primer: give a range across
studies rather than one number, cite the study behind a figure, and never state
a figure the tools did not return. The screening test is a screening of basic
number skills, not a diagnosis, and the server scores no one.
