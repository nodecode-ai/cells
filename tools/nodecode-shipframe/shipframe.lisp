;;; shipframe -- ShipFrame's coding-workflow skills as one pack (skills/ at 2d0a246, MIT)
(in-package :nodecode.evolved)

(define-skill shipframe
  "ShipFrame's 44 coding-workflow skills as one pack: how to refresh project context, discover a feature, plan it, implement and verify it, review it, prove a release, and open the PR. Read this first to pick the skill that fits."
  :origin "github.com/juanitourquiza/shipframe"
  ;; ShipFrame turns a development request into a repeated delivery lifecycle:
  ;;
  ;;   Refresh context -> Discover requirements -> Plan work -> Implement ->
  ;;   Verify -> Review -> Release evidence -> PR/MR
  ;;
  ;; This cell carries the 44 skills that walk that chain. Each is a folder in
  ;; this cell's own skills/ beside this file, with a SKILL.md in it, and each
  ;; one is a skill of its own: the workflow skills below are named exactly as
  ;; their folder is, so (help 'code-review), (help 'tdd) or (help 'create-pr)
  ;; reads one whole. Nothing here runs: a skill is prose, and what it says to
  ;; run is the project's own git, gh or test command.
  ;;
  ;; Walk the chain (each one names the next):
  ;;
  ;;   project-memory-refresh, then feature-discovery, then plan-expert or
  ;;   planning-features, then implement-task, then tdd or e2e-verify, then
  ;;   code-review, then release-checklist or project-release with
  ;;   deploy-evidence, then create-pr.
  ;;
  ;; The whole pack, by what it is for:
  ;;
  ;;   Context and memory: project-memory-refresh, project-memory-init,
  ;;     memory-curator, project-profile, init-project, generate-readme, handoff.
  ;;   Discovery and planning: feature-discovery, plan-expert, planning-features,
  ;;     create-task, create-issue, research.
  ;;   Building: implement-task, tdd, bug-diagnosis, codebase-design,
  ;;     domain-modeling, dependency-upgrade.
  ;;   Review and audit: code-review, api-contract-review, security-review,
  ;;     security-hardening, a11y-auditor, client-copy-review, evidence-audit.
  ;;   Verification and release: e2e-verify, proof-runner, release-checklist,
  ;;     project-release, frontend-release, backend-release, deploy-evidence.
  ;;   Delivery: create-pr.
  ;;   Design: design-expert, design-system-setup, design-system-docs.
  ;;   Incident and debugging: incident-response, mcp-debugging.
  ;;   Wiki: wiki-init, wiki-query, wiki-sync, wiki-forge.
  ;;   Documentation: live-docs.
  ;;
  ;; How the skills expect to be read:
  ;;
  ;; - Guidance by default. Reading a skill does not authorize writing:
  ;;   project-memory-init, init-project, generate-readme and the wiki skills
  ;;   each say which files they may create (AGENTS.md, WIKI.md,
  ;;   .shipframe/memory/) and ask first.
  ;; - Evidence over claims. deploy-evidence, release-checklist, evidence-audit
  ;;   and proof-runner refuse to call work done without a command that ran and
  ;;   its output; proof-runner runs only the Verify: commands a checklist
  ;;   spells out, and asks before a destructive one.
  ;; - Never store secrets. memory-curator, generate-readme and init-project
  ;;   read keys, never values, and keep no credentials in what they write.
  ;; - Ask when a rule matters. feature-discovery and plan-expert ask before
  ;;   they decide; code-review reports what it did not check.
  ;;
  ;; The skills were written for several harnesses and name a few hosts:
  ;; Claude Code, Codex CLI and OpenCode, ClickUp tasks, Engram and the Context
  ;; MCP. Read those as optional: where a host is missing, the skill says what
  ;; still works locally (a local test manifest instead of a ClickUp task, git
  ;; instead of gh). The pack's own rules are the ones that matter here.
  )
