;;;; nodecode-openltm.asd --- long-term memory kept in this home, for the model to use.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openltm' section, reads this home's memory back, registers the
;;;; eight openltm_* tools and /openltm, puts the manual on (help :openltm),
;;;; folds each finished turn on the :frame point, and adds its paragraph to
;;;; every request's harness sections.
;;;;
;;;; A Nodecode rewrite of RohiRIK/OpenLtm's Hermes plugin (see NOTICE).
;;;; Every dependency is already in the serving image.

(defsystem "nodecode-openltm"
  :description "OpenLTM memory: local long-term memory the model stores facts in, links and recalls"
  :license "MIT"
  :version "0.1.0"
  :author "RohiRIK (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openltm/test"))))

(defsystem "nodecode-openltm/test"
  :description "OpenLTM tests. Registered into the shared nodecode.test registry; RUN-OPENLTM-TESTS filters by the OPENLTM-CELL- name prefix."
  :license "MIT"
  :author "RohiRIK (original), Nodecode (rewrite)"
  :depends-on ("nodecode-openltm" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openltm-tests)))
