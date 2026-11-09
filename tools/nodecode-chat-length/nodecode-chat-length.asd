;;;; nodecode-chat-length.asd --- a notice when a chat nears its context window.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode cell, NOT part of the organism core: nothing in Nodecode names
;;;; this system. A rewrite of MadeByBrain/hermes-chat-length's Hermes plugin
;;;; (see NOTICE). The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `chat-length' section, hooks the :ROUND point so a notice
;;;; rides the request in which a threshold is crossed, and registers the
;;;; chat_length tool, /chat-length and the manual on (help :chat-length).

(defsystem "nodecode-chat-length"
  :description "A notice when a chat is getting long, with an opener for a fresh one"
  :license "MIT"
  :author "MadeByBrain (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-chat-length/test"))))

(defsystem "nodecode-chat-length/test"
  :description "Chat-length tests. Registered into the shared nodecode.test registry; RUN-CHAT-LENGTH-TESTS filters by the CHAT-LENGTH-CELL- name prefix."
  :license "MIT"
  :author "MadeByBrain (original), Nodecode (rewrite)"
  :depends-on ("nodecode-chat-length" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-chat-length-tests)))
