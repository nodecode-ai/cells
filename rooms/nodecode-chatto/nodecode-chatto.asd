;;;; nodecode-chatto.asd --- Chatto as a Nodecode channel.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when a channels.chatto
;;;; section is present: the kit finds this folder's system by name, calls
;;;; its START-CHANNEL, and draws the room with its own host. Rewritten from
;;;; jrimmer's Hermes Agent plugin; see NOTICE.

(defsystem "nodecode-chatto"
  :description "Talk to nodecode through a Chatto server"
  :author "jrimmer (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-channel-kit")
  :serial t
  :components ((:file "package")
               (:file "chatto"))
  :in-order-to ((test-op (test-op "nodecode-chatto/test"))))

(defsystem "nodecode-chatto/test"
  :description "Chatto lane tests, registered under the CHATTO-CELL- prefix."
  :author "jrimmer (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-chatto" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-chatto-tests)))
