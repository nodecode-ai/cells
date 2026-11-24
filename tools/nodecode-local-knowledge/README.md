# nodecode-local-knowledge

Local knowledge: index local skills, scripts, runbooks and docs so a question routes to the file to read first. A Nodecode rewrite of [stepanov1975/hermes-local-knowledge](https://github.com/stepanov1975/hermes-local-knowledge) by [stepanov1975](https://github.com/stepanov1975).

A search answers whole artifacts, not passages: the file the agent should open
next, where it is, and why it answered. Nothing leaves the machine — the walk
is a walk of your own directories, and the record of what you asked is one
tab-separated file beside the index.

## Get it

Tell the agent "add the local-knowledge cell", or run:

```sh
nodecode add nc://local-knowledge
```

## Set it up

The cell needs the directories to index and nothing else. A section in
`~/.nodecode/config.jsonc`:

```jsonc
{
  "local-knowledge": {
    "roots": ["~/repos/ops", "~/.nodecode/skills"],
    "max_depth": 6,
    "max_files": 4000,
    "max_index_age_seconds": 300
  }
}
```

`roots` is required; a question asked with none refuses and says so. The rest
have defaults you can leave alone. `/settings`, the page's Settings tab and
"set the local-knowledge roots to ~/repos/ops" all write the same section.

What a root holds is read by rule: a `SKILL.md` is a skill, a file whose
name or directory says runbook is a runbook, other prose is a doc, a known
extension makes a script or a config. Directory names like `.git`,
`node_modules` and `venv` are skipped; `exclude_dir_names` adds your own.

## Use it

The model gets five tools:

| Tool | What it answers |
| --- | --- |
| `knowledge_search` | the artifacts a question routes to, best first, with an optional type filter |
| `knowledge_get` | one artifact by id: title, path, size, headings, the start of its text, and what sits beside it |
| `knowledge_neighbors` | the edges out of an artifact: siblings, a skill's support documents, documents that name it |
| `knowledge_feedback` | one rating — useful, missing, noisy, wrong_artifact, stale, other — kept locally |
| `knowledge_usage_report` | what was asked, what found nothing, and what was said about the hits |

Ask it in your own words: "which runbook covers the backup drill?" It answers
with the file to read, and the agent reads that file before acting on it. The
index is rebuilt when it is older than `max_index_age_seconds`, or at once
with `rebuild: true` after you add a file.

From the shell:

```
/knowledge backup drill     search
/knowledge rebuild          walk the roots again now
/knowledge                  what is indexed, and search from the panel
```

`(help :local-knowledge)` carries the same guidance into the conversation.

## What it does not do

It does not read the text of a script or a config into the index: those are
found by name and path, while skills, runbooks and docs are read for their
headings and words. It does not follow a source change by itself — an
artifact edited a minute ago looks the same until a walk happens. And it is a
router, not an answer: a hit is a hint to open the file, never a quote from
it.
