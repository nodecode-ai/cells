;;;; nodecode-heartbeat.asd --- wakes and watchers: a check-in that wakes its own session.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of electricsheephq/heartbeat2 for Nodecode: one tool ("pulse")
;;;; the model arms, one /heartbeat command the operator types, and a ticker
;;;; thread that checks every standing watch out of band. A time watch is a
;;;; wake: a prompt into the session that armed it. A probe watch (file, url,
;;;; command) costs no model turn until the thing it watches changes or
;;;; breaks. See src/CELLS.md for the cell contract.

(defsystem "nodecode-heartbeat"
  :description "Wakes and watchers: a timed check-in, or a file, url or command that wakes its session when it changes"
  :license "Apache-2.0"
  :author "electricsheephq (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "watch")
               (:file "run")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-heartbeat/test"))))

(defsystem "nodecode-heartbeat/test"
  :description "The heartbeat cell's tests, registered in the shared nodecode.test registry; RUN-HEARTBEAT-TESTS filters by the HEARTBEAT-CELL- prefix."
  :license "Apache-2.0"
  :author "electricsheephq (original), Nodecode (rewrite)"
  :depends-on ("nodecode-heartbeat" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-heartbeat-tests)))
