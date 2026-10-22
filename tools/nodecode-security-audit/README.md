# nodecode-security-audit

Cloudflare's [security-audit skill](https://github.com/cloudflare/security-audit-skill) as a Nodecode cell: multi-phase security audits with independently verified, machine-readable findings. Works with any model.

## Get it

Tell the agent "add the security audit cell", or run `nodecode add nc://security-audit`.

## Use it

Ask a security question, or ask it to audit a codebase. The agent finds `security-audit` in its index and reads it with `(help 'security-audit)`.

- **Guidance mode** is the default: focused reviews, triage and security questions. It writes no files.
- **Full audit mode** runs only when you ask for a full audit, a pen test or a report: six phases, from reconnaissance to a written report. Every candidate goes to a fresh verifier, and the findings are written as JSON checked against `report-schema.json`.
- Target code runs only in a sandbox with no network, a scratch home and resource limits. Without one, it reads the source and reports what it could not check.
- The two validators need Node.js: `node skill/validate-findings.cjs`.

## What is in it

| File | What it is |
| --- | --- |
| `security-audit.lisp` | The skill: `SKILL.md` as a prose `define-skill`. |
| `skill/` | Cloudflare's `skills/security-audit/` folder at `c1c8a8c`, unchanged: the phase guides, the attack-surface guides, the report schema and the validators. |

## License

MIT. The skill is Copyright (c) 2025-2026 Cloudflare, Inc.: see `LICENSE` and `NOTICE`.
