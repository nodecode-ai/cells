;;;; nodecode-skill-court.asd --- put a written skill on trial.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. A rewrite of saatvik333's
;;;; skill-court (see NOTICE): a skill's cases, the pairs of runs judged on
;;;; them -- once with the skill, once without it -- the paired score with
;;;; its 95% interval, and the quarantine list of the skills that hurt.

(defsystem "nodecode-skill-court"
  :description "Skill Court: paired with/without evidence for a skill, a 95% verdict and a quarantine list"
  :license "MIT"
  :version "0.1.0"
  :author "saatvik333 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "court")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-skill-court/test"))))

(defsystem "nodecode-skill-court/test"
  :description "Tests for Skill Court. Registered into the shared nodecode.test registry; RUN-SKILL-COURT-TESTS filters by the SKILL-COURT-CELL- prefix."
  :license "MIT"
  :depends-on ("nodecode-skill-court" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-skill-court-tests)))
