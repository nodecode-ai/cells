;;;; nodecode-follow-through.asd --- an agent never ends a turn on a promise.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the follow-through plugin for Hermes Agent (see NOTICE):
;;;; the plugin's one middleware and its one tool, as a cell that hooks the
;;;; round a model answer is recorded in and reads it for a promise.

(defsystem "nodecode-follow-through"
  :description "Hold an answer that promises work nothing is running, and make it do the work or launch it"
  :license "MIT"
  :author "Agenziamaia (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "promise")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-follow-through/test"))))

(defsystem "nodecode-follow-through/test"
  :description "Follow-through tests: the lifecycle, the promise patterns, the round hook, the tool and /follow-through."
  :license "MIT"
  :author "Agenziamaia (original), Nodecode (rewrite)"
  :depends-on ("nodecode-follow-through" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-follow-through-tests)))
