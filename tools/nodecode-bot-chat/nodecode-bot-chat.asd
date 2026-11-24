;;;; nodecode-bot-chat.asd --- a concise bot style, and a guard that only ever blocks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of Tormen28/hermes-bot-chat-agent: the same style
;;;; contract and the same seven guard rules, on one :TOOL hook, one harness
;;;; section and one advice instead of two Hermes hooks. See README.md beside
;;;; this file and NOTICE for what came from where.

(defsystem "nodecode-bot-chat"
  :description "Bot chat: a concise bot style and a block-only safety guard, per session"
  :license "MIT"
  :author "Tormen28 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-bot-chat/test"))))

(defsystem "nodecode-bot-chat/test"
  :description "Bot chat's tests. Registered into the shared nodecode.test registry; RUN-BOT-CHAT-TESTS filters by the BOT-CHAT- name prefix."
  :license "MIT"
  :author "Tormen28 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-bot-chat" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-bot-chat-tests)))
