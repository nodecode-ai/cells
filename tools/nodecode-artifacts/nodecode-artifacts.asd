;;;; nodecode-artifacts.asd --- Artifacts: the private library and its guest links.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell, not part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `artifacts' section and registers the six tools,
;;;; /artifacts and the manual topic.
;;;;
;;;; A Nodecode rewrite of dmmeteo/hermes-artifacts (see NOTICE).

(defsystem "nodecode-artifacts"
  :description "Artifacts: a private artifact library - publish HTML, Markdown, SVG or images, and hand out expiring guest links"
  :author "dmmeteo (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-artifacts/test"))))

(defsystem "nodecode-artifacts/test"
  :description "Artifacts tests: the six tools over a stubbed service, and the refusals."
  :license "MIT"
  :author "dmmeteo (original), Nodecode (rewrite)"
  :depends-on ("nodecode-artifacts" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-artifacts-tests)))
