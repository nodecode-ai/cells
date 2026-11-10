;;;; nodecode-witness.asd --- evidence-gated learning: the skills the agent keeps, graded.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/, and the gateway calls START-CELL, which
;;;; reads the `witness' config section, advises NLK:RECORD-DEFINITIONS,
;;;; hooks the :TOOL point, registers the `witness' tool and /witness.

(defsystem "nodecode-witness"
  :description "Evidence-gated learning: grades each skill the agent keeps and quarantines the unearned ones"
  :license "MIT"
  :author "BenCoy09 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "grade")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-witness/test"))))

(defsystem "nodecode-witness/test"
  :description "Witness tests. Registered into the shared nodecode.test registry; RUN-WITNESS-TESTS filters by the WITNESS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-witness" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-witness-tests)))
