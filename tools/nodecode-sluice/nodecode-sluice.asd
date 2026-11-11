;;;; nodecode-sluice.asd --- a staged copy of a workspace, reviewed and applied.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The .asd sits inside its folder; the system is named by the file. The
;;;; loader finds START-CELL in package NODECODE-SLUICE, which the gateway
;;;; calls after recovery; the section, the tools, the command and the help
;;;; topic come off the one NLE:DEFINE-CELL declaration in cell.lisp.

(defsystem "nodecode-sluice"
  :description "Runs the agent's commands in a staged copy of a workspace and applies only what you approve"
  :license "MIT"
  :author "orjinameh (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "stage")
               (:file "diffing")
               (:file "applying")
               (:file "classify")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-sluice/test"))))

(defsystem "nodecode-sluice/test"
  :description "Sluice tests: the stage, the review, the apply and the gate, offline."
  :license "MIT"
  :author "orjinameh (original), Nodecode (rewrite)"
  :depends-on ("nodecode-sluice" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-sluice-tests)))
