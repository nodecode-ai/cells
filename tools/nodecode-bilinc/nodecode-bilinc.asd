;;;; nodecode-bilinc.asd --- Bilinc memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `bilinc' section, registers the four bilinc_* tools and
;;;; /bilinc, hooks recall onto a turn and the optional turn capture onto the
;;;; frame point.
;;;;
;;;; A Nodecode rewrite of ReARCLabs's bilinc Hermes plugin (see NOTICE).

(defsystem "nodecode-bilinc"
  :description "Bilinc memory: versioned long-term facts with recall, remember, revise and forget"
  :license "MIT"
  :version "0.1.0"
  :author "ReARCLabs (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-bilinc/test"))))

(defsystem "nodecode-bilinc/test"
  :description "Bilinc tests. Registered into the shared nodecode.test registry; RUN-BILINC-TESTS filters by the BILINC-CELL- name prefix."
  :license "MIT"
  :author "ReARCLabs (original), Nodecode (rewrite)"
  :depends-on ("nodecode-bilinc" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-bilinc-tests)))
