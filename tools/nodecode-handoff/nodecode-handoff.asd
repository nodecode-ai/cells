;;;; nodecode-handoff.asd --- a validated, immutable handoff a fresh session reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `handoff' section and registers the semantic_handoff tool,
;;;; /handoff and (help :handoff).
;;;;
;;;; State is one folder per handoff under the Nodecode home: a markdown page
;;;; and its manifest, written once and never rewritten. Nothing here calls a
;;;; model or a network.

(defsystem "nodecode-handoff"
  :description "A validated, immutable handoff for a fresh session"
  :author "yuzushi-dev (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-handoff/test"))))

(defsystem "nodecode-handoff/test"
  :description "Handoff tests. Registered into the shared nodecode.test registry; RUN-HANDOFF-TESTS filters by the HANDOFF-CELL- name prefix."
  :author "yuzushi-dev (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-handoff" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-handoff-tests)))
