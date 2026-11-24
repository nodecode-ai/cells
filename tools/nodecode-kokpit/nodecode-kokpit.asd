;;;; nodecode-kokpit.asd --- Kokpit: your own Markdown vault, through Nodecode.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL, which reads the `kokpit'
;;;; section, registers the five kokpit_* tools and /kokpit, and checks that
;;;; the vault is a folder this machine can read.
;;;;
;;;; A Nodecode rewrite of sahindemirbas's Kokpit Hermes plugin (see NOTICE).

(defsystem "nodecode-kokpit"
  :description "Kokpit: your own Markdown vault - brief, threads, capture, night pass"
  :license "MIT"
  :version "0.1.0"
  :author "sahindemirbas (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kokpit/test"))))

(defsystem "nodecode-kokpit/test"
  :description "Kokpit tests. Registered into the shared nodecode.test registry; RUN-KOKPIT-TESTS filters by the KOKPIT-CELL- name prefix."
  :license "MIT"
  :author "sahindemirbas (original), Nodecode (rewrite)"
  :depends-on ("nodecode-kokpit" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kokpit-tests)))
