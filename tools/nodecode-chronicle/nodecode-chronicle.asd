;;;; nodecode-chronicle.asd --- Chronicle memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `chronicle' section, registers the nine chronicle_* tools and
;;;; /chronicle, and hooks the :frame point so every finished turn is captured.
;;;;
;;;; A Nodecode rewrite of indigokarasu's Chronicle Hermes plugin (see NOTICE).

(defsystem "nodecode-chronicle"
  :description "Chronicle memory: local-first beliefs and captured turns, kept in one file on this machine"
  :license "MIT"
  :version "0.1.0"
  :author "indigokarasu (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-chronicle/test"))))

(defsystem "nodecode-chronicle/test"
  :description "Chronicle tests. Registered into the shared nodecode.test registry; RUN-CHRONICLE-TESTS filters by the CHRONICLE-CELL- name prefix."
  :license "MIT"
  :author "indigokarasu (original), Nodecode (rewrite)"
  :depends-on ("nodecode-chronicle" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-chronicle-tests)))
