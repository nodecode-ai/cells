;;;; nodecode-hyatlas.asd --- HyAtlas memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `hyatlas' section, registers the four hyatlas_* tools and
;;;; /hyatlas, recalls memories at the start of every turn and sends each
;;;; finished turn to the server.
;;;;
;;;; A Nodecode rewrite of tuancookiez-hub's HyAtlas-Memory Hermes plugin
;;;; (see NOTICE).

(defsystem "nodecode-hyatlas"
  :description "HyAtlas memory: an eight-layer self-hosted memory server, recalled and captured through its HTTP API"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "tuancookiez-hub (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hyatlas/test"))))

(defsystem "nodecode-hyatlas/test"
  :description "HyAtlas tests. Registered into the shared nodecode.test registry; RUN-HYATLAS-TESTS filters by the HYATLAS-CELL- name prefix."
  :license "Apache-2.0"
  :author "tuancookiez-hub (original), Nodecode (rewrite)"
  :depends-on ("nodecode-hyatlas" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hyatlas-tests)))
