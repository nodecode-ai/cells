;;;; nodecode-iris-chat.asd --- private messaging for this agent, over the iris CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `iris-chat' section, registers the seven tools and the /iris
;;;; command, hooks the :ROUND point so a turn's first request says how many
;;;; messages wait unread, puts the manual on (help :iris), and carries the
;;;; standing paragraph in every request.
;;;;
;;;; The one thing outside this folder is the iris program itself: a release
;;;; binary the user installs (brew install sirius/iris/iris, or the project's
;;;; installer), which holds this identity's keys and message database. See
;;;; NOTICE: this folder is a rewrite of irislib/iris-chat-rs's Hermes plugin.

(defsystem "nodecode-iris-chat"
  :description "iris: an agent's own private messaging identity, over the iris program"
  :license "MIT"
  :version "0.1.0"
  :author "irislib (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-iris-chat/test"))))

(defsystem "nodecode-iris-chat/test"
  :description "iris-chat tests. Registered into the shared nodecode.test registry; RUN-IRIS-CHAT-TESTS filters by the IRIS-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "irislib (original), Nodecode (rewrite)"
  :depends-on ("nodecode-iris-chat" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-iris-chat-tests)))
