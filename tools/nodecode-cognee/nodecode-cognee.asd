;;;; nodecode-cognee.asd --- Cognee memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cognee' section, registers the four cognee_* tools and
;;;; /cognee, and talks to the Cognee server the section names.
;;;;
;;;; A Nodecode rewrite of topoteretes's Cognee Hermes plugin (see NOTICE).

(defsystem "nodecode-cognee"
  :description "Cognee memory: recall, remember and forget a knowledge graph on a local or remote Cognee server"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "topoteretes (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cognee/test"))))

(defsystem "nodecode-cognee/test"
  :description "Cognee tests. Registered into the shared nodecode.test registry; RUN-COGNEE-TESTS filters by the COGNEE-CELL- name prefix."
  :license "Apache-2.0"
  :author "topoteretes (original), Nodecode (rewrite)"
  :depends-on ("nodecode-cognee" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cognee-tests)))
