;;;; nodecode-sentinel.asd --- the loop trap, credential redaction, a ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral: presence is enabled, and the gateway calls this system's
;;;; START-CELL after recovery. The cell declares one :tool hook, one advice
;;;; on NLE:SUBMIT, one slash command and one help topic, and derives its
;;;; stop from them; nothing in the core names it.

(defsystem "nodecode-sentinel"
  :description "Refuses repeated tool calls, redacts credentials and bounds tool results"
  :author "SaltProphet (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-sentinel/test"))))

(defsystem "nodecode-sentinel/test"
  :description "Sentinel tests: the trap, the redaction, the bound, the command, the lifecycle."
  :license "MIT"
  :depends-on ("nodecode-sentinel" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-sentinel-tests)))
