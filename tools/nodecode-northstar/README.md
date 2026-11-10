# nodecode-northstar

Northstar: reads the instruction files a session is told to obey and reports
the ones that disagree — the same directive written in two files, a
contradiction between two of them, a path that resolves nowhere. A Nodecode
rewrite of [mrcoxis-2002/hermes-northstar](https://github.com/mrcoxis-2002/hermes-northstar)
by [mrcoxis-2002](https://github.com/mrcoxis-2002).

A session is governed by prose, not code: the home's `AGENTS.md`,
`SYSTEM.md`, `SOUL.md` and `config.jsonc`, the project's `AGENTS.md`,
`CLAUDE.md`, `.cursorrules` and `README.md` in its folder and the folders
above it, and any `SKILL.md` under the home's `skills/`. Two of those can
say opposite things, and the model obeys whichever it read last. Northstar
reads them all and says where they part company. Read-only: it edits,
repairs and restarts nothing.

## Get it

Tell the agent "add the northstar cell", or run `nodecode add nc://northstar`.

## Set it up

Nothing to set up; every member of the section has a default:

```jsonc
"northstar": {
  "files": [],          // more names to read beside the standard ones
  "extra_dirs": [],     // more folders, each of its text files
  "max_parents": 4,     // folders above the project to read too
  "max_findings": 40    // how long one report may be
}
```

`files` adds to `AGENTS.md`, `CLAUDE.md`, `.cursorrules` and `README.md`;
`extra_dirs` is read for every `.md`, `.txt`, `.json`, `.jsonc`, `.yaml`,
`.yml` and `.toml` in it, a relative name under the home. A value of the
wrong shape refuses at start and the folder says so on `/cells` and in the
notices block.

## Use it

- `/northstar` audits everything; `/northstar project` one scope
  (`full`, `home`, `project`, `skills`), `/northstar json` the JSON form,
  `/northstar status` what it reads and its limit.
- `(northstar:audit)` and `(northstar:audit :scope "home" :output-format "json")`
  answer the same through eval.
- `northstar_audit` is the tool the model calls, with `scope` and
  `output_format`.

The three categories are the original's:

| Category | What it detects |
| --- | --- |
| `duplicate` | the same directive line in two files, frontmatter and metadata lines excluded |
| `contradiction` | one file asking for approval where another forbids it — an error, or a warning when the two sides name different scopes |
| `stale_reference` | a path in an instruction file that resolves nowhere, URLs, markdown links, code spans, code blocks and examples skipped |

```
Northstar coherence audit: 1 error, 0 warnings across 3 sources
  ~/.nodecode/AGENTS.md (instruction, 12 lines)
  ~/work/AGENTS.md (instruction, 8 lines)
  ~/work/CLAUDE.md (instruction, 5 lines)
  [error] contradiction · ~/work/AGENTS.md:2
      evidence: "ask for confirmation" in AGENTS.md vs "without confirmation" in CLAUDE.md
      contradiction: AGENTS.md allows what CLAUDE.md forbids
      -> choose one directive and remove the other, or say which scope each governs
```

Sources are listed with what kind each is — `instruction`, `config`, `skill`
— and how many lines it holds; a report longer than `max_findings` says how
many findings it left out. Contradictions are checked inside one file and
between every pair of instruction files; a skill and a config are read for
duplicates and dangling paths, not for contradictions.

A finding's confidence says how much to trust it: `medium` for a
contradiction with no scope words on either side, `low` when the two sides
name different scopes (both may be right) or when a path may be a
documentation reference. The checks are heuristics over words, not a parser.

## What the rewrite leaves out

The plugin's second tool, `northstar_status`, answered a static placeholder;
`(northstar:status)` here says what this audit actually reads. Its
`pre_llm_call` hook injected the cached last audit's first three errors into
a session's first turn; here the audit runs when it is asked. See `NOTICE`.

Run `.github/test-cell.sh tools/nodecode-northstar` from the cells repository.
