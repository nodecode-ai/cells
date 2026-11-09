;;;; nodecode-headroom.asd --- a memory guard for heavy shell jobs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of seongj-un's Hermes plugin seongj-un/hermes-headroom as a
;;;; Nodecode cell: one :TOOL hook around the eval dispatch, one tool, one
;;;; slash command and one manual topic. Nothing in the core names this
;;;; system.

(defsystem "nodecode-headroom"
  :description "Queues, caps and watches heavy shell jobs so a small machine never runs out of memory"
  :license "MIT"
  :author "seongj-un (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-headroom/test"))))

(defsystem "nodecode-headroom/test"
  :description "Headroom tests: the classifier, the rewrites, the hook, the tool and the command."
  :license "MIT"
  :depends-on ("nodecode-headroom" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-headroom-tests)))
