;;;; nodecode-steward.asd --- durable project ownership, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A peripheral cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `steward' section, registers the five
;;;; model-facing tools cell.lisp declares, hooks the :ROUND point so a
;;;; session bound to a project reads that project's line at each request,
;;;; puts (help :steward) on the manual and registers /steward. Everything
;;;; it learns lives in one JSON file under the Nodecode home; it runs the
;;;; user's own check commands, and nothing else.

(defsystem "nodecode-steward"
  :description "Project stewardship: per-project autonomy, measured health cycles and human-approved initiatives"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-steward/test"))))

(defsystem "nodecode-steward/test"
  :description "Steward tests. Registered into the shared nodecode.test registry; RUN-STEWARD-TESTS filters by the STEWARD-CELL- name prefix."
  :license "Apache-2.0"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-steward" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-steward-tests)))
