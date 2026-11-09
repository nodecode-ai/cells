;;;; nodecode-inbox.asd --- one read-only view of everything in flight.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of NousResearch's `inbox' plugin for Hermes Agent: `hermes
;;;; inbox' printed what a Hermes profile had in flight, and this cell prints
;;;; the same five sections for a Nodecode home, read-only. See NOTICE.

(defsystem "nodecode-inbox"
  :description "One read-only view of everything in flight: pending, running, scheduled, open"
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "snapshot")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-inbox/test"))))

(defsystem "nodecode-inbox/test"
  :description "Inbox tests: the lifecycle, the five sections over stubbed sources, the tool, /inbox."
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode-inbox" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-inbox-tests)))
