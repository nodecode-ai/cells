;;;; nodecode-jev-curator.asd --- typed skill-relation judgments and consolidation plans.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `jev-curator' section, puts (help :jev-curator) on the manual,
;;;; registers /curator, and leaves the model four functions it calls through
;;;; eval.
;;;;
;;;; Rewritten from anpicasso's hermes-jev-curator (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-jev-curator"
  :description "Typed skill-relation judgments and hash-bound consolidation plans over a SKILL.md library"
  :license "MIT"
  :author "anpicasso (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "skills")
               (:file "candidates")
               (:file "plans")
               (:file "judge")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jev-curator/test"))))

(defsystem "nodecode-jev-curator/test"
  :description "The jev-curator cell's tests. Registered into the shared nodecode.test registry; RUN-JEV-CURATOR-TESTS filters by the JEV-CURATOR-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-jev-curator" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jev-curator-tests)))
