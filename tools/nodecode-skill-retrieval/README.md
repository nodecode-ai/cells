# nodecode-skill-retrieval

Skill Retrieval: BM25 ranking over a library of SKILL.md skills, a tool and a /command for asking by hand, and each turn's best matches injected into its own request. A Nodecode rewrite of [moonlight-lupin/agent-skills](https://github.com/moonlight-lupin/agent-skills) by [moonlight-lupin](https://github.com/moonlight-lupin).

A skill library that fits in the prompt is a skill library nobody searches. Past a
few dozen skills, their descriptions crowd out the work, and a model reading all
of them still misses the one that fits. This cell ranks them instead: names and
descriptions are weighted by BM25 (Okapi, Lucene's IDF), the few that match what
the turn is about are shown with the folder of the file holding their steps, and
the rest stay on disk.

## Get it

Tell the agent "add the skill-retrieval cell", or run `nodecode add nc://skill-retrieval`.

## Set it up

Name the folders that hold SKILL.md files - an imported skill library, a shared
checkout, your own procedures as prose - in `/settings` (or the Settings tab):

```json
"skill-retrieval": { "roots": ["~/skill-libraries"] }
```

`roots` is the one member without a default. Every other member is optional:

| Setting | Default | What it does |
| --- | --- | --- |
| `roots` | (required) | folders scanned recursively for `SKILL.md` |
| `top_k` | `6` | how many skills one search or one turn carries |
| `inject` | `true` | whether each turn carries the matches in its own request |
| `triggers` | `true` | index a skill's trigger phrases and tags beside its description |

## Use it

The model gets one tool, `skill_search`, with a `query` and an optional
`top_k`; it answers the best matches, best first:

```
- **Deploy a service** (deploy): Ship a docker container to production with one command.
```

The id in parentheses is the skill's folder below its root, so the file is
`<root>/deploy/SKILL.md`: the descriptions say which one to read, and the file
holds the steps.

`/skill-search WORDS` asks the same question by hand; typed alone it says how
many skills the roots hold.

Every turn, before the request goes out, the turn's own input is ranked and the
top matches ride that request under `## Retrieved skills`, once per turn. Turn
`inject` off to keep only the tool and the command.

### What is indexed, and what is not

A skill is a file named `SKILL.md`, anywhere below a root, whose frontmatter
carries `name` and `description`. Its folder name is its id. `triggers` and
`tags` are indexed beside the description - a skill can fire on "ship it" even
when its description says "deploy" - and only the description is ever shown.
Nothing leaves the machine: the index is built in memory from the files under
the roots, and rebuilt only when a file's mtime or size moves.

### What the rewrite leaves out

No reranking service is called, so the ranking is BM25 alone. The original's
system-prompt compaction has no counterpart here, because the corpus is the
roots' own files rather than the harness's skill index. See `NOTICE`.
