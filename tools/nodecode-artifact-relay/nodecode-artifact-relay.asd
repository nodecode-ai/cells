;;;; nodecode-artifact-relay.asd --- Artifact Relay: long results as private links.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell, not part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `artifact-relay' section and registers the two tools and
;;;; /artifact-relay.
;;;;
;;;; A Nodecode rewrite of eloktev/hermes-artifact-relay (see NOTICE).

(defsystem "nodecode-artifact-relay"
  :description "Artifact Relay: publish long Markdown or HTML results as private links, and read their source back"
  :author "eloktev (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "setup")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-artifact-relay/test"))))

(defsystem "nodecode-artifact-relay/test"
  :description "Artifact Relay tests: the two tools over a stubbed wire, the sign-in flow, the refusals."
  :license "MIT"
  :author "eloktev (original), Nodecode (rewrite)"
  :depends-on ("nodecode-artifact-relay" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-artifact-relay-tests)))
