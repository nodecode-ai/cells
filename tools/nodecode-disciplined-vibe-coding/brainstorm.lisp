;;; brainstorm.lisp --- Use after intake when a confirmed direction has approach-level design choices. 2-3 options per fork, batch independent forks into one stop, docs on demand. (Lineage: superpowers/brainstorming, godmode/rationale)
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; The procedure, whole, as the original prose skill wrote it (skills/brainstorm/SKILL.md, MIT).
;;; The guides it names sit in guides/ beside this file: economy-ladder.md,
;;; to-spec.md, writing-plan.md, wayfinder.md. Read them from there.
;;;
(in-package :nodecode.evolved)

(define-skill brainstorm
  "Use after intake when a confirmed direction has approach-level design choices. 2-3 options per fork, batch independent forks into one stop, docs on demand. (Lineage: superpowers/brainstorming, godmode/rationale)"
  :origin "github.com/bmtrnavsky/disciplined-vibe-coding"
  ;; phase: Define
  ;;
  ;; # Brainstorm -- shape the approach before committing to it
  ;;
  ;; ## Prime directive
  ;; Understanding precedes solutions. Approach forks get aired here, once.
  ;;
  ;; ## Steps (in order)
  ;; 1. Restate the confirmed direction in one sentence.
  ;; 2. List the approach forks. None? Say so; move to Plan.
  ;; 3. Per fork: 2-3 approaches -- what it is, main cost, main benefit, when it wins.
  ;; 4. **Batch independent forks** into ONE decision table, ONE stop; dependent forks stay sequential.
  ;; 5. **Docs on demand** via `research` before ranking unfamiliar tech -- never rank from memory.
  ;; 6. Recommendation + why per fork; his preference where it matters.
  ;;
  ;; ## Done when
  ;; - Every fork has one chosen answer (his or stated-reason yours).
  ;; - Closing plain paragraph "what we'll build and how" delivered.
  ;;
  ;; ## Cognitive traps
  ;; - "One question per turn is polite" -- batched stops respect his time.
  ;; - "Memory is API knowledge" -- step 5 exists for that.
  ;;
  ;; ## Guardrails
  ;; - Max 3 options per fork. No implementation, no task lists.
  ;; - Picks against recommendation? One evidence-backed objection, then follow.
  ;;
  ;; ## Rung scaling (economy ladder)
  ;;
  ;; One-liner and micro-spec: brainstorm only when a real approach fork
  ;; exists; otherwise say so and move to Plan. Full-spec and above: full
  ;; steps with batched forks. Docs-on-demand applies at every rung.
  ;;
  )
