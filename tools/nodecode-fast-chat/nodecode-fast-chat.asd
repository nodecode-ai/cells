;;;; nodecode-fast-chat.asd --- a session of its own on the model you name.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of iyoda/hermes-fast-chat: the original adds one Hermes CLI
;;;; command that opens a CLI or TUI session on an explicit model, provider and
;;;; Fast tier without touching the profile's defaults. Here the thing it
;;;; opens is a session of this organism, pinned to that target, and the
;;;; gateway's default model stays where it was. The cell contributes one
;;;; slash command and one manual topic; it runs no thread.

(defsystem "nodecode-fast-chat"
  :description "Open a session on the model you name, leaving the defaults alone"
  :license "MIT"
  :version "0.1.0"
  :author "iyoda (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fast-chat/test"))))

(defsystem "nodecode-fast-chat/test"
  :description "Fast Chat's slice: the lifecycle, the argument line, a pinned session."
  :license "MIT"
  :author "iyoda (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fast-chat" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fast-chat-tests)))
