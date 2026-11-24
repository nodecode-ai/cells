;;;; nodecode-severian.asd --- Severian memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `severian' section, registers the five severian_* tools and
;;;; /severian, and starts the worker that writes each finished turn into a
;;;; Severian server's store.
;;;;
;;;; A Nodecode rewrite of Sahil-SS9/Severian's Hermes plugin (see NOTICE).

(defsystem "nodecode-severian"
  :description "Severian memory: cited long-term recall from a local Severian server"
  :license "MIT"
  :version "0.1.0"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-severian/test"))))

(defsystem "nodecode-severian/test"
  :description "Severian tests. Registered into the shared nodecode.test registry; RUN-SEVERIAN-TESTS filters by the SEVERIAN-CELL- name prefix."
  :license "MIT"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-severian" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-severian-tests)))
