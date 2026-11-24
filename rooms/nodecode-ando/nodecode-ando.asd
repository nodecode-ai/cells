;;;; nodecode-ando.asd --- Ando workspace messages, as a room.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Ando-Corporation/hermes-ando (see NOTICE). Nothing in
;;;; Nodecode names this system: the folder loader loads it at boot when the
;;;; directory sits under ~/.nodecode/cells/, and the gateway calls
;;;; START-CELL after recovery, which reads the `ando' section, redeems the
;;;; invitation it names once, and polls Ando for the messages this agent
;;;; was invited to answer.

(defsystem "nodecode-ando"
  :description "Ando: answer the workspace messages your Ando agent is invited to, from a Nodecode session"
  :license "MIT"
  :author "Ando-Corporation (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "connection")
               (:file "inbox")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ando/test"))))

(defsystem "nodecode-ando/test"
  :description "Ando cell tests: invite redemption, the MCP framing, the sweep and the reply, offline."
  :license "MIT"
  :depends-on ("nodecode-ando" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ando-tests)))
