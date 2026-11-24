;;;; nodecode-ordaprompt.asd --- route an ask to a kept topic, or say it abstains.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of aska-digital/protean-ordaprompt, whose only agent-facing
;;;; surface was a Python CLI. Optional ASDF cell, not part of the organism
;;;; core: the folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ordaprompt' section and registers the ordaprompt_route tool,
;;;; the /route command and (help :ordaprompt). Nothing here reaches the
;;;; network: the scoring backend is the cell's own, and the receipts are
;;;; files the operator names.

(defsystem "nodecode-ordaprompt"
  :description "Fail-closed topic and session routing: one batch comparison, three bands, hashes-only receipts"
  :license "MIT"
  :author "aska-digital (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ordaprompt/test"))))

(defsystem "nodecode-ordaprompt/test"
  :description "Ordaprompt tests. Registered into the shared nodecode.test registry; RUN-ORDAPROMPT-TESTS filters by the ORDAPROMPT-CELL- name prefix."
  :license "MIT"
  :author "aska-digital (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ordaprompt" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ordaprompt-tests)))
