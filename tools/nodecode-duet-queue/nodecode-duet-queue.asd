;;;; nodecode-duet-queue.asd --- a background task queue with human approval gates.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of benfirad's hermes-plugin-antigravity-duet (MIT).
;;;; The queue is a JSON file under the Nodecode home: a task a model adds
;;;; waits in "awaiting_approval" until the operator approves it with
;;;; /duet approve ID, and a worker runs it through the operator's own agent
;;;; CLI, one task at a time, its output kept on the task.

(defsystem "nodecode-duet-queue"
  :description "A background task queue: the model enqueues, the operator approves, a CLI runs it"
  :license "MIT"
  :version "0.1.0"
  :author "benfirad (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-duet-queue/test"))))

(defsystem "nodecode-duet-queue/test"
  :description "Duet queue tests, offline: the store, the approval gate, the worker over a stubbed runner."
  :license "MIT"
  :depends-on ("nodecode-duet-queue" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-duet-queue-tests)))
