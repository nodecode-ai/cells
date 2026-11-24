;;;; nodecode-iris-chat-channel.asd --- Iris Chat: one approved contact, in a room.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: the folder loader loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `channels.iris' section,
;;;; starts one poll lap, and registers /iris-chat-channel, its manual topic and the
;;;; iris_chat_send tool.
;;;;
;;;; The one thing outside this folder is the iris CLI, which the user
;;;; installs (https://github.com/irislib/iris-chat-channel-rs) and which holds the
;;;; identity, the keys and the messages. See NOTICE: this folder rewrites
;;;; irislib/iris-chat-channel-channels' hermes plugin.

(defsystem "nodecode-iris-chat-channel"
  :description "Iris Chat: talk to nodecode through one approved Iris contact"
  :license "MIT"
  :version "0.1.0"
  :author "irislib (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-iris-chat-channel/test"))))

(defsystem "nodecode-iris-chat-channel/test"
  :description "Iris Chat tests. Registered into the shared nodecode.test registry; RUN-IRIS-CHAT-CHANNEL-TESTS filters by the IRIS-CHAT-CHANNEL-CELL- name prefix."
  :license "MIT"
  :author "irislib (original), Nodecode (rewrite)"
  :depends-on ("nodecode-iris-chat-channel" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-iris-chat-channel-tests)))
