# nodecode-certarc

CertArc: free CISM, CISSP, CISA, AAISM and AAIA practice questions, exam outlines and study-guide links, inside Nodecode. A Nodecode rewrite of [CertArc/certarc-mcp](https://github.com/CertArc/certarc-mcp) by [CertArc](https://github.com/CertArc).

## Get it

Tell the agent "add the certarc cell", or run `nodecode add nc://certarc`.

## Set it up

Nothing to set. The cell calls CertArc's public endpoint, `https://certarc.com/mcp/grok`, and that endpoint needs no account and no key.

Optional, in the `certarc` section of `~/.nodecode/config.jsonc`:

| Key | What it does |
| --- | --- |
| `program` | the exam `/certarc` asks about when none is named: `cism`, `cissp`, `cisa`, `aaism` or `aaia`. Empty asks each time. |
| `base_url` | where every call goes, when you answer through a proxy of your own. |
| `timeout_seconds` | one call's ceiling. |

## Use it

Ask for a practice question and name the exam: "test me on CISSP", "give me a CISM question", "what does the AAIA exam cover", "find a CertArc guide on incident response". The agent finds four tools:

| Tool | What it answers |
| --- | --- |
| `certarc_practice_question` | one scenario question, its options and its question id, never the answer; a set is 10 questions |
| `certarc_check_answer` | whether the learner's choice was right, the correct option and CertArc's explanation for every option; a confident wrong answer comes back as a blind spot |
| `certarc_exam_domains` | the exam's domains and their weights, with links to CertArc's guides and free practice set |
| `certarc_learning_guides` | guide titles and links, matched on the words in a title |

A session goes: the question, the choice and how sure they were (confident, unsure or guess), then the check and the explanation. Answers may carry a `startUrl`, CertArc's free assessment: the agent offers it once, and the learner signs in themselves. `/certarc cism` asks the first CISM question; `/certarc` alone says what the cell points at.

Nothing here is a score. The questions are CertArc's own scenarios, not ISACA or ISC2 questions, and no answer is recorded.

## What it is

The original is configuration only: a manifest pointing a harness at CertArc's hosted MCP server, and one skill. This cell calls that server itself, in Common Lisp, one JSON-RPC `tools/call` per tool over `nlk:http`, and lays each answer out as the lines a model reads. No Node, no Python, no key, and no vendored copy of anything.

| File | What it is |
| --- | --- |
| `cell.lisp` | the section, the four tools, `/certarc`, and the one refusal |
| `nodecode-certarc.asd` | the system and its test system |
| `test/cell-test.lisp` | the tests, every request a stubbed `nlk:http` |

## License

MIT. The original is Copyright (c) 2026 The CertArc authors: see `LICENSE` and `NOTICE`. That covers this rewrite, not CertArc's hosted server, its questions, its explanations or its name.
