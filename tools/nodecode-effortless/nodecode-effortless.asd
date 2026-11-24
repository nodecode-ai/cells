;;;; nodecode-effortless.asd --- pick the reasoning effort for a message, and set it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `effortless' section, registers the pick_effort tool, puts
;;;; (help :effortless) on the manual and registers /effortless.
;;;;
;;;; A rewrite of ivcained/effortless-hermes for Hermes (see NOTICE). Every
;;;; dependency rides with nodecode: the judges are one POST over NLK:HTTP.

(defsystem "nodecode-effortless"
  :description "Effortless: judges a message and sets this session's reasoning effort"
  :license "MIT"
  :author "ivcained (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "judge")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-effortless/test"))))

(defsystem "nodecode-effortless/test"
  :description "Effortless tests. Registered into the shared nodecode.test registry; RUN-EFFORTLESS-TESTS filters by the EFFORTLESS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-effortless" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-effortless-tests)))
