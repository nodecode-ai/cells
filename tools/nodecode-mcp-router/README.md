# nodecode-mcp-router

MCP Router: pick the MCP tools that fit a query inside a context budget, and describe each choice. A Nodecode rewrite of [Pinutss/jev-mcp-router](https://github.com/Pinutss/jev-mcp-router) by [Pinutss](https://github.com/Pinutss).

With it the model has one more tool:

```lisp
mcp_select {"query": "read a local file, then search GitHub issues",
            "tools": [{"id": "filesystem_read", "description": "Read local files.",
                       "tags": ["files", "read"], "tokens": 80,
                       "permissions": ["read_files"], "fallback_id": "docs_search"},
                      {"id": "github_search", "description": "Search GitHub issues.",
                       "tokens": 120, "permissions": ["read_github"]}],
            "max_tools": 1, "budget_tokens": 250}
;; => {"decision": "select",
;;     "selected": [{"id": "filesystem_read", "score": 0.866, "tokens": 80,
;;                   "reasons": ["relevance=1.00", "tokens=80", "efficiency=0.56",
;;                               "max_calls=8"], "permissions": ["read_files"], ...}],
;;     "fallback": {"id": "docs_search", ...},
;;     "total_tokens": 80,
;;     "reasons": ["best_match", "relevance=1.00", ...],
;;     "rejected": [{"id": "github_search", "reason": "low_relevance"}]}
```

and the same selection from `eval`:

```lisp
(mcp-router:select-tools "read a local file"
                          (list (mcp-router:make-tool-profile
                                 :id "filesystem_read" :name "Filesystem Read"
                                 :description "Read local files." :tokens 80)))
```

When many MCP tools are available and only a budgeted subset should be offered, the
tool ranks the catalog by the words it shares with the query, fits the best into
`max_tools` and `budget_tokens`, explains each choice, names every rejection with its
cause, abstains when nothing clears the floor, and previews one fallback. It never runs
a tool and it never dials: the ranking is lexical and local, so the same catalog and
the same query always answer the same selection.

Typed as a command, `/mcp-router` says what the cell does and, given a query, selects
over a small demo catalog so the ranking is visible without a config. `(help :mcp-router)`
carries the manual while the cell runs, and every request's help section carries one
line naming the tool.

## Get it

Tell the agent "add the mcp-router cell", or run `nodecode add nc://mcp-router`.

## Set it up

Nothing is required: the selector is local and needs no key. The section is on unless
it says `"enabled": false`, and each member has a default:

```jsonc
{
  "mcp-router": {
    "enabled": true,
    "max_tools": 8,          // how many tools a selection may keep
    "budget_tokens": 800,    // the context budget the kept tools must fit
    "min_relevance": 0.15,   // the lexical floor, 0.0 to 1.0
    "omit_rejected": false,  // true to leave the rejected list out of the answer
    "extra_stopwords": []    // words the lexical match ignores
  }
}
```

`max_tools`, `budget_tokens` and `min_relevance` are the ceiling and the floor a call
that names neither gets; a call's own members win. A `min_relevance` outside 0.0 to 1.0
refuses the start, and the folder's line in `/cells` says so until it is fixed.

## Use it

- **A catalog of MCP tools.** Pass what your MCP servers offer — `id`, `name`,
  `description`, `server`, `tags`, `tokens`, `permissions`, `enabled`, `fallback_id`,
  `max_calls`, `scope` (all but `id` optional) — and the query you are about to run.
- **A budget you must not exceed.** `max_tools` and `budget_tokens` cut the list; what
  was cut is in `rejected` with `max_tools` or `budget` beside it.
- **A decision you can read out.** `reasons` on each kept tool (`relevance`, `tokens`,
  `efficiency`, `max_calls`) and on the answer itself; `abstain_reason` when nothing was
  kept (`no_candidate`, `no_fallback`, `fallback_disabled`).
- **A tool that just failed.** `failed_tool_id` makes it a replacement choice: the
  failed tool's own `fallback_id` is tried first, and the decision reads `fallback`.
- **Policies before scores.** `required_permissions` and `forbidden_permissions` are
  declared by the caller: a candidate lacking a required permission, or holding a
  forbidden one, is rejected whatever its words. Selection is not authorization — a
  permission is what the catalog declares, and a query, a tool or a model cannot add
  one.

## What it does not do

- **No execution.** It picks and explains; the caller runs the tool.
- **No remote judge.** The original can hand the ranking to JEV or to an LLM gateway
  vendor when keys are configured (`JEV_*`, `GATEWAY_*`); this cell is the local mode,
  which is the original's default. There is no key, no endpoint and no network call.
- **No HTTP server and no demo page.** The original also serves `/v1/select` and a demo
  page; here the tool, `/mcp-router` and `(mcp-router:select-tools ...)` from `eval`
  take that place.
- **No Windows-specific work**: nothing here touches the filesystem, a process or a port.

## Test

```sh
.github/test-cell.sh tools/nodecode-mcp-router
```

MIT licensed; see [NOTICE](NOTICE).
