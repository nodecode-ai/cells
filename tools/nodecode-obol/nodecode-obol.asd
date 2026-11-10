;;;; nodecode-obol.asd --- invoices and Stripe payment links, from the chat.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `obol' section, registers the seven invoice tools and the
;;;; /obol command, and puts the manual on (help :obol).
;;;;
;;;; Nothing outside this folder is needed: Stripe is read with the image's own
;;;; nlk:http. See NOTICE: this folder is a rewrite of UEddy/Obol.

(defsystem "nodecode-obol"
  :description "Obol: invoices and Stripe payment links, with a local ledger and risk checks"
  :license "MIT"
  :version "0.1.0"
  :author "UEddy (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-obol/test"))))

(defsystem "nodecode-obol/test"
  :description "Obol tests. Registered into the shared nodecode.test registry; RUN-OBOL-TESTS filters by the OBOL-CELL- name prefix."
  :license "MIT"
  :author "UEddy (original), Nodecode (rewrite)"
  :depends-on ("nodecode-obol" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-obol-tests)))
