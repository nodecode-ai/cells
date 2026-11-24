;;;; nodecode-duty.asd --- what is waiting, running and failing in this home.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of xiaoyaner0201's `duty-board' plugin for Hermes Agent: a
;;;; read-only board of conversations whose answer waits on the person,
;;;; turns still running or left without an answer, background results not
;;;; yet delivered and cron jobs in trouble. See NOTICE.

(defsystem "nodecode-duty"
  :description "What is waiting, running and failing in this home, read-only"
  :license "Apache-2.0"
  :author "xiaoyaner0201 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-duty/test"))))

(defsystem "nodecode-duty/test"
  :description "Duty tests: the lifecycle, the sections over stubbed sources, the tool, /duty."
  :license "Apache-2.0"
  :author "xiaoyaner0201 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-duty" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-duty-tests)))
