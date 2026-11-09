;;;; nodecode-sgme.asd --- SGME memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `sgme' section, registers the fourteen sgme_* tools and /sgme,
;;;; and starts the worker that appends each finished turn to SGME.
;;;;
;;;; A Nodecode rewrite of freehul/sgme's Hermes memory-provider plugin
;;;; (see NOTICE).

(defsystem "nodecode-sgme"
  :description "SGME memory: tagged long-term recall, skills, wiki and care signals from a self-hosted engine"
  :license "MIT"
  :version "0.1.0"
  :author "freehul (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-sgme/test"))))

(defsystem "nodecode-sgme/test"
  :description "SGME tests. Registered into the shared nodecode.test registry; RUN-SGME-TESTS filters by the SGME-CELL- name prefix."
  :license "MIT"
  :author "freehul (original), Nodecode (rewrite)"
  :depends-on ("nodecode-sgme" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-sgme-tests)))
