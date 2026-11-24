;;;; nodecode-tripwire.asd --- every refused action, one line, so the owner sees it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of percyhendrux123-prog/hermes-tripwire (a Hermes Agent
;;;; plugin) as a Nodecode cell: the same record of what an agent was refused,
;;;; written on the seam Nodecode already has (a :TOOL point hook), and the
;;;; same digest of what is new. No dependencies beyond nodecode.

(defsystem "nodecode-tripwire"
  :description "Records every refused tool call to a local log, and digests what was stopped"
  :license "MIT"
  :version "0.1.0"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "log")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tripwire/test"))))

(defsystem "nodecode-tripwire/test"
  :description "Tripwire tests: the record, the drill, the digest, the command and the tool."
  :license "MIT"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :depends-on ("nodecode-tripwire" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tripwire-tests)))
