# nodecode-shipframe

ShipFrame: AI coding workflows for teams that plan, prove and ship, as 44 skills a Nodecode agent reads. A Nodecode rewrite of [juanitourquiza/shipframe](https://github.com/juanitourquiza/shipframe) by [juanitourquiza](https://github.com/juanitourquiza).

## Get it

Tell the agent "add the shipframe cell", or run `nodecode add nc://shipframe`.

## Use it

Ask for the work, not for the skill: "plan this feature", "review this branch", "is the release really done?", "open a draft PR". The agent reads `shipframe` — the hub skill this cell adds — finds the skill that fits in its index, and reads it with `(help 'code-review)`, `(help 'tdd)`, `(help 'deploy-evidence)` and so on.

The skills walk one chain, each naming the next:

```text
Refresh context → Discover requirements → Plan work → Implement → Verify → Review → Release evidence → PR/MR
```

- **Context and memory**: `project-memory-refresh`, `project-memory-init`, `memory-curator`, `project-profile`, `init-project`, `generate-readme`, `handoff`.
- **Discovery and planning**: `feature-discovery`, `plan-expert`, `planning-features`, `create-task`, `create-issue`, `research`.
- **Building**: `implement-task`, `tdd`, `bug-diagnosis`, `codebase-design`, `domain-modeling`, `dependency-upgrade`.
- **Review and audit**: `code-review`, `api-contract-review`, `security-review`, `security-hardening`, `a11y-auditor`, `client-copy-review`, `evidence-audit`.
- **Verification and release**: `e2e-verify`, `proof-runner`, `release-checklist`, `project-release`, `frontend-release`, `backend-release`, `deploy-evidence`.
- **Delivery and docs**: `create-pr`, `live-docs`, the `wiki-*` skills, `design-expert`, `design-system-setup`, `design-system-docs`, `incident-response`, `mcp-debugging`.

Every one is guidance: reading it writes nothing. The skills that do create files say so and ask first — `project-memory-init` keeps a small Markdown memory in the repository at `.shipframe/memory/` and only publishes a Draft PR when told to; `init-project` and `generate-readme` write `AGENTS.md` or a README on request. Shell work is the project's own: `git`, `gh`, and whatever the test suite is. Nothing here calls a service, holds a key, or needs a setting, so the cell has no section to configure.

## In it

| File | What it is |
| --- | --- |
| `shipframe.lisp` | The hub skill: the chain, the pack by purpose, and the rules the skills share. |
| `skills/` | ShipFrame's `skills/` at `2d0a246`, unchanged: 44 folders, a `SKILL.md` in each, read where they sit. |

A `SKILL.md` under a cell folder is a skill as it stands: Nodecode takes its frontmatter `name` and `description` for the index and the text after them for `(help 'name)`. So the 44 are read in place, not converted and not copied, and the ones naming a sibling file — `tdd/mocking.md`, `codebase-design/DEEPENING.md`, `domain-modeling/ADR-FORMAT.md`, `project-memory-init/scripts/project-memory-state.sh` — find it beside them.

## What is not here

ShipFrame is also an installer for Claude Code, Codex CLI and OpenCode (symlinked skills, a prompt router, an orchestrator guard), a set of subagent profiles, project technology packs, templates, and an opt-in AI PR/MR reviewer that runs in GitHub Actions or GitLab CI. Those belong to those hosts or to a CI job, and a Nodecode cell does not install into other agents; the skills, which are what a user works with, are all here.

## License

MIT. The skills are Copyright (c) 2026 Juan Urquiza: see `LICENSE` and `NOTICE`.
