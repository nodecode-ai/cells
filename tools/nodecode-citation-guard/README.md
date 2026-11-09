# nodecode-citation-guard

Citation Guard: catches fabricated, mismatched and retracted references before you rely on them. A Nodecode rewrite of [ChienNguyen23/hermes-citation-guard](https://github.com/ChienNguyen23/hermes-citation-guard) by [ChienNguyen23](https://github.com/ChienNguyen23).

Language models invent references that look right. This cell checks every
citation against the registries that issue the identifiers: a DOI or PMID or
arXiv id that does not exist, an id glued to a paper it does not belong to, a
wrong year or first author, works found in no registry at all, and retractions
and expressions of concern (Crossref's update notices, which include the
Retraction Watch data, and PubMed's publication types).

## Get it

Tell the agent "add the citation-guard cell", or run `nodecode add nc://citation-guard`.

## Set it up

Nothing is required. Two optional settings, in the `citation-guard` section of
`~/.nodecode/config.jsonc`:

| Setting | What it is for |
| --- | --- |
| `contact_email` | Sent to Crossref and NCBI as the polite-pool contact address, and to nothing else. |
| `ncbi_api_key` | A free NCBI key, sent only to NCBI, so its rate limit counts requests per key instead of per address. |
| `max_items` | How many references one check reads (40 by default). |
| `request_timeout_s` | How long one registry request may take (8 seconds by default). |

No key is needed to use it, and no account.

## Use it

- **The tool.** `verify_citations` takes plain text -- a reference list, a
draft with a References section, or BibTeX -- and answers one block per
reference: verified, details differ, id does not exist, id points to a
different work, retracted, expression of concern, not found in any registry,
or not checked. The model is told to call it before presenting a bibliography.
- **The command.** `/citecheck <file>` reads a `.md`, `.txt`, `.bib` or
  `.docx` (an absolute path, or `~/...`), and `/citecheck <references>` checks
text you paste. The full report comes back to you.
- **The manual.** `(help :citation-guard)` answers what the verdicts mean and
  how to work with them while the cell runs.

A verdict of *not checked* is a registry that did not answer -- never a claim
that a reference is fake. Readings are cached in memory for the run, so
re-checking the same bibliography costs nothing.

## What it reaches

Four public, read-only scholarly APIs: `api.crossref.org`, `doi.org` (for
DataCite and the other DOI agencies Crossref does not index),
`eutils.ncbi.nlm.nih.gov` and `export.arxiv.org`. Only the reference itself
is sent -- an id, or a title with its first author and year -- never the rest
of the conversation, a file, or anything about this machine. Every request
carries an honest `citation-guard/0.1.0` user agent.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The contract, the `verify_citations` tool, `/citecheck`, the manual, the section |
| `extract.lisp` | References out of text: ids, entries, BibTeX |
| `sources.lisp` | Crossref, doi.org, PubMed, arXiv, and the in-memory cache |
| `verify.lisp` | The judgement of one reference, and the report text |
| `test/` | Offline tests: the registries are stubbed, nothing dials |

## Tests

```sh
.github/test-cell.sh tools/nodecode-citation-guard
```

Offline: a canned Crossref, PubMed and arXiv answer through the stubbed
`nlk:http`, so the run asserts on what the cell asked and what it made of the
answer.

## Credits

MIT, Copyright (c) 2026 Chien Nguyen. A rewrite of
[ChienNguyen23/hermes-citation-guard](https://github.com/ChienNguyen23/hermes-citation-guard)
at commit `6916b2ae7f561765883c57b32441db049e246113`; see [NOTICE](NOTICE).
