;;;; nodecode-channel-chatwork.asd --- Chatwork as a Nodecode channel.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when a channels.chatwork
;;;; section is present: the kit finds this folder's system by name, calls
;;;; its START-CHANNEL, and draws the room with its own host. Rewritten from
;;;; TakeshiTGAL's Hermes Agent plugin; see NOTICE.

(defsystem "nodecode-channel-chatwork"
  :description "Chatwork: talk to Nodecode from a Chatwork room"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-channel-kit" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-channel-chatwork/test"))))

(defsystem "nodecode-channel-chatwork/test"
  :description "Chatwork lane tests, registered under the CHATWORK-CELL- prefix."
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-channel-chatwork" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-chatwork-tests)))
