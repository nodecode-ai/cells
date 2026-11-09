# nodecode-holographic

Holographic Memory: a fact store kept on this machine, recalled into each turn. A Nodecode rewrite of [itpartypattaya/hermes-plugin-holographic](https://github.com/itpartypattaya/hermes-plugin-holographic) by [itpartypattaya](https://github.com/itpartypattaya).

The original keeps its facts in a SQLite database and re-ranks them with
holographic vectors. This cell keeps the same kind of fact in one file of
Lisp data and finds them by words, so it needs no database, no key and no
network: what it knows never leaves the machine.

## Get it

Tell the agent "add the holographic cell", or run `nodecode add nc://holographic`.

## Set it up

Nothing is required. The facts live in one file under this home; the
section tunes it:

| Key | Default | What it does |
| --- | --- | --- |
| `db_path` | `<home>/holographic-facts.sexp` | the file the facts live in |
| `default_trust` | `0.5` | the trust a new fact starts with, 0 to 1 |
| `min_trust` | `0.3` | facts below this are not recalled |
| `recall` | `on` | search each turn's message and put the facts it recalls into that request |
| `recall_facts` | `5` | the most facts one turn's recall injects |
| `recall_max_chars` | `800` | the most characters the injected block may take |

## Use it

Talk, and it remembers. Each turn's own message is searched against the
facts already stored, and the ones it recalls ride that request under
`# Holographic memory`, each with the id (`#3`) that rates it later. Nothing
is written automatically unless the model writes it: memory is something the
model adds, not a transcript.

The model has two tools:

| Tool | What it does |
| --- | --- |
| `fact_store` | `add` one short sentence; `search` by words, or as of a date; `list`, `history` of a fact, `stats`; `change` when it changed, `end` when it stopped being true, `forget` when the person asks you to |
| `fact_feedback` | `helpful` raises a fact's trust, `unhelpful` lowers it, `confirmed` records that the person said it again |

and you have `/holographic`:

| Command | What it does |
| --- | --- |
| `/holographic` | how many facts, where the file is, what recall does |
| `/holographic search QUERY` | ask the store by hand |
| `/holographic add ONE SENTENCE` | remember something now |

From eval the same verbs are Lisp: `(holographic:recall "…")`,
`(holographic:remember "…" :keywords '("synonym"))`,
`(holographic:forget 3)`, `(holographic:facts-text (holographic:recall "q"))`.

A fact is one short standalone sentence, and the keywords are what makes it
findable: words someone would ask for it by that are not in its text —
synonyms, other word forms, other languages. Trust starts at 0.5, and a
fact below `min_trust` is not recalled. A `change` keeps the text it
replaced, so `history` can answer "what did it say before", and `end` keeps
a fact that was true until a date out of recall without destroying it.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, its one condition, `(holographic:…)` |
| `store.lisp` | the fact, the file, the words, the verbs |
| `cell.lisp` | the section, the two tools, `/holographic`, the round |

## Credits

A rewrite of [itpartypattaya/hermes-plugin-holographic](https://github.com/itpartypattaya/hermes-plugin-holographic),
at commit `f75c636`, MIT. See [NOTICE](NOTICE) for what was rewritten and
what was left out, and [LICENSE](LICENSE) for the licence it keeps.
