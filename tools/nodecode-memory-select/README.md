# nodecode-memory-select

Memory selection: keep the retrieved memories that fit the query and the token budget. A Nodecode rewrite of [Pinutss/jev-memory-selector](https://github.com/Pinutss/jev-memory-selector) by [Pinutss](https://github.com/Pinutss).

A retrieved memory list usually arrives too long: facts about other projects,
another session's preferences, a note that expired last month. The original
plugin sat between retrieval and the prompt and kept what was worth sending.
This cell makes the same decision in the image, with no Python and no network
at all.

| Tool | What it does |
| --- | --- |
| `memory_select` | Takes what the next turn is about and the candidates you already retrieved, and answers the ones to keep: each with its score, the tokens it costs and the reasons it was kept, then every memory left behind with the one reason it was. |

From eval, the same call:

```lisp
(memory-select:select "where does the app keep its data"
                      '((:id "db" :text "PostgreSQL, on the build host" :importance 0.7)
                        (:id "ui" :text "The settings page uses a dark theme")))
```

A memory answers with `id` and `text` (or `content`), and may carry
`created_at` (ISO-8601), `importance` (0 to 1), `tags`, `scope`, `expires_at`
and a retrieval `similarity`. A plist from eval reads the same way.

## Get it

Tell the agent "add the memory-select cell", or run `nodecode add nc://memory-select`.

## Set it up

Nothing is required: the defaults keep ten memories of about 4000 tokens, and
every one of them is a member of the `memory-select` section in `/settings` or
in `config.jsonc`:

```jsonc
{ "memory-select": { "max_results": 10, "max_tokens": 4000, "min_relevance": "0.15" } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `max_results` | `10` | how many memories one selection may keep (at least 1) |
| `max_tokens` | `4000` | the token budget one selection may spend (at least 1) |
| `min_relevance` | `"0.15"` | the share of the query's words a memory must carry, 0 to 1 (`"0"` keeps everything) |
| `recency_days` | `"14"` | the half-life in days of a memory's freshness |
| `dedupe` | `"0.8"` | how much of its words two memories must share to keep only the first, 0 to 1 (`"0"` turns the test off) |
| `redact` | `true` | answer a key-shaped string in a memory as a marker instead of the key |

The three decimal members are text in the section, which carries no decimal
type; a value that is not a number reads as its default. A member of the wrong
shape refuses the start and says which one. Then
`(restart-cells "nodecode-memory-select")`.

## Use it

Ask a question that leans on what was remembered — "which database did we pick,
and where does it run?" — and an agent holding too many memories calls
`memory_select` with the list and the question, then sends the selected ones.

How it decides:

- **Relevance** is lexical and deliberately simple: the share of the query's
  words (stopwords dropped, in English and French) that the memory's text and
  tags carry. A memory sharing no word with the query falls under
  `min_relevance` and is dropped as `low_relevance`.
- **Freshness** halves every `recency_days`; **importance** is the caller's
  own 0 to 1. The score is `0.5 × relevance + 0.3 × freshness + 0.2 ×
  importance`, and ties go to the newer memory, then the lower id, so the same
  list answers the same way every time.
- **The budget** is estimated at about four characters to the token. A memory
  that does not fit is dropped (`budget`) rather than cut in half, and a list
  past `max_results` is stopped (`max_items`).
- **Duplicates**: two memories that share `dedupe` of their words keep the
  first, and the second says `duplicate_of:ID`. A memory in another `scope`
  (`out_of_scope`), past its `expires_at` (`expired`), or carrying an id
  already seen (`duplicate_input`) is dropped before scoring.
- **Contradictions** are a warning, not a drop: MySQL against PostgreSQL, or
  SQLite against either, keeps both and marks the later one
  `possible_conflict:ID`.
- **Keys** that appear inside a memory answer as `[REDACTED_API_KEY]`,
  `[REDACTED_BEARER]`, `[REDACTED_PASSWORD]` or `[REDACTED_PRIVATE_KEY]`, so a
  selection read out loud never repeats one.

What it does not do: it retrieves nothing (give it the candidates), it writes
no prose, and it needs no key. `(help :memory-select)` answers the same
guidance the model reads; `memory-select::*primer*` is the text, so a layer
can reword it.
