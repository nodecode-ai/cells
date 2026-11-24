;;;; nodecode-gbrain.asd --- ambient context from a GBrain brain.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gbrain' section, registers the gbrain_recall tool and /gbrain,
;;;; hooks the :ROUND point so a turn's first request carries the brain's
;;;; pages for the conversation, and puts the manual on (help :gbrain).
;;;;
;;;; A rewrite of aean0x's Hermes plugin of the same purpose (see NOTICE):
;;;; it speaks GBrain's HTTP MCP endpoint directly, the way that plugin did.

(defsystem "nodecode-gbrain"
  :description "Ambient context from a GBrain brain: its pages for a turn, before the model asks"
  :license "MIT"
  :version "0.1.0"
  :author "aean0x (original), Nodecode (rewrite)"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gbrain/test"))))

(defsystem "nodecode-gbrain/test"
  :description "GBrain tests. Registered into the shared nodecode.test registry; RUN-GBRAIN-TESTS filters by the GBRAIN-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-gbrain" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gbrain-tests)))
