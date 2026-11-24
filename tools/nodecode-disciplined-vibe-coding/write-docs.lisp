;;; write-docs.lisp --- Use whenever writing docs/notes/changelogs/plans/roadmaps/prose for the user. Pragmatic ASD-STE100. Governs question register too. (Lineage: hermes/creative-simple-english, AminBlg/SimpleEnglish, posthog/writing-simplified-technical-english)
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; The procedure, whole, as the original prose skill wrote it (skills/write-docs/SKILL.md, MIT).
;;; The guides it names sit in guides/ beside this file: economy-ladder.md,
;;; to-spec.md, writing-plan.md, wayfinder.md. Read them from there.
;;;
(in-package :nodecode.evolved)

(define-skill write-docs
  "Use whenever writing docs/notes/changelogs/plans/roadmaps/prose for the user. Pragmatic ASD-STE100. Governs question register too. (Lineage: hermes/creative-simple-english, AminBlg/SimpleEnglish, posthog/writing-simplified-technical-english)"
  :origin "github.com/bmtrnavsky/disciplined-vibe-coding"
  ;; phase: Any
  ;;
  ;; # Write docs -- impossible to misread
  ;;
  ;; ## Prime directive
  ;; A tired nonexpert reader lands the right meaning on the first read.
  ;;
  ;; ## Rules (pragmatic STE)
  ;; One idea per sentence (25 words; 20 for instructions). Active voice. One word per action. Numbered steps, verb first. Define terms at first use. must/cannot for requirements; can for capability; never "should" -- should hides a decision.
  ;;
  ;; ## Untouchables
  ;; Code, identifiers, commands, paths, config keys, error strings, URLs, backtick content. Byte-exact.
  ;;
  ;; ## Modes
  ;; Pragmatic (default) for artifacts. Principles-only (one-idea sentences + defined jargon, conversational) for questions.
  ;;
  ;; ## Guardrails
  ;; Never edit code/commands/errors while "fixing" docs. Self-check: 25+ word sentence? undefined term? lurking "should"?
  ;;
  ;; ## Rung scaling (economy ladder)
  ;;
  ;; The rung sets artifact weight, not prose quality -- STE rules hold at every
  ;; rung. One-liner: chat prose only, nothing retained. Micro-spec and above:
  ;; changelog entries where `version` requires them. Project: retained notes
  ;; and roadmap prose.
  ;;
  )
