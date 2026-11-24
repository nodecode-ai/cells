;;;; nodecode-fashio.asd --- Fashio's e-commerce fashion photography, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `fashio' section, registers the tools cell.lisp declares and
;;;; puts (help :fashio) on the manual.
;;;;
;;;; A rewrite of fashiolabs/fashio-agent-plugin (see NOTICE).

(defsystem "nodecode-fashio"
  :description "Fashio: on-model fashion photos, try-on, flat lays and product videos on your own Fashio key"
  :license "MIT"
  :version "0.1.0"
  :author "fashiolabs (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fashio/test"))))

(defsystem "nodecode-fashio/test"
  :description "Fashio cell tests. Registered into the shared nodecode.test registry; RUN-FASHIO-TESTS filters by the FASHIO-CELL- name prefix."
  :license "MIT"
  :author "fashiolabs (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fashio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fashio-tests)))
