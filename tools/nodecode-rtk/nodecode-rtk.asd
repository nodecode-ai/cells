;;;; nodecode-rtk.asd --- shell commands rewritten through the rtk binary.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of kerrz2020/hermes-rtk-rewrite's Hermes plugin, which
;;;; bridges every terminal tool call to `rtk rewrite' before the command runs.
;;;; Here the same bridge is the core's :TOOL point: the command of a shell
;;;; call, or of an eval snippet that is one, is rewritten on its way to the
;;;; shell.

(defsystem "nodecode-rtk"
  :description "Rewrite shell commands through the rtk binary before they run"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "kerrz2020 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-rtk/test"))))

(defsystem "nodecode-rtk/test"
  :description "RTK cell tests. Registered into the shared nodecode.test registry; RUN-RTK-TESTS filters by the RTK-CELL name prefix so the core suite is never re-run."
  :license "Apache-2.0"
  :author "kerrz2020 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-rtk" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-rtk-tests)))
