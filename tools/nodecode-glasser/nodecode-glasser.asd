;;;; nodecode-glasser.asd --- Glasser's data endpoints as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `glasser' section and registers the seven glasser_* tools, the
;;;; /glasser command and the (help :glasser) topic.
;;;;
;;;; A Nodecode rewrite of glasser-ai/hermes-plugin-glasser (see NOTICE).

(defsystem "nodecode-glasser"
  :description "Glasser: search, inspect and run 1,000+ pay-per-call data endpoints over its MCP server"
  :license "MIT"
  :version "0.1.0"
  :author "glasser-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-glasser/test"))))

(defsystem "nodecode-glasser/test"
  :description "Glasser tests. Registered into the shared nodecode.test registry; RUN-GLASSER-TESTS filters by the GLASSER-CELL- name prefix."
  :license "MIT"
  :author "glasser-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-glasser" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-glasser-tests)))
