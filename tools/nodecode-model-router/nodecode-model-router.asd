;;;; nodecode-model-router.asd --- one decision a task, from a catalog of models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `model-router' section, registers the model_route tool,
;;;; puts /route and (help :model-router) up, and answers the decisions the
;;;; tool makes. Nothing here dials a vendor: a row names the environment
;;;; variable that may hold its key, and the ranking is arithmetic.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting - or symlinking -
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-model-router"
  :description "Picks an LLM for a task under quality, cost and latency limits, and explains the choice"
  :license "MIT"
  :version "0.1.0"
  :author "Pinutss (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "catalog")
               (:file "router")
               (:file "judge")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-model-router/test"))))

(defsystem "nodecode-model-router/test"
  :description "Tests for the model-router cell; RUN-MODEL-ROUTER-TESTS filters the shared registry by the MODEL-ROUTER-CELL- prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Pinutss (original), Nodecode (rewrite)"
  :depends-on ("nodecode-model-router" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "router-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-model-router-tests)))
