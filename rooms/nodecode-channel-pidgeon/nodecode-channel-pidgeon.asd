;;;; nodecode-channel-pidgeon.asd --- Pidgeon as a Nodecode channel.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when a channels.pidgeon
;;;; section is present: the kit finds this folder's system by name, calls
;;;; its START-CHANNEL, and draws the thread with its own host. Rewritten from
;;;; pidgeon-ai's Hermes Agent plugin; see NOTICE.

(defsystem "nodecode-channel-pidgeon"
  :description "Talk to nodecode by email through your Pidgeon inbox"
  :author "pidgeon-ai (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-channel-kit")
  :serial t
  :components ((:file "package")
               (:file "pidgeon"))
  :in-order-to ((test-op (test-op "nodecode-channel-pidgeon/test"))))

(defsystem "nodecode-channel-pidgeon/test"
  :description "Pidgeon lane tests, registered under the PIDGEON-CELL- prefix."
  :author "pidgeon-ai (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-channel-pidgeon" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pidgeon-tests)))
