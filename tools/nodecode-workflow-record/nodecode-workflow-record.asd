;;;; nodecode-workflow-record.asd --- record a browser workflow once, keep it as data.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of NousResearch's workflow-record plugin for Hermes Agent:
;;;; the same recording model (one JSON file of steps under the home, secret
;;;; fields masked as {SECRET:name}, one tab, top-frame navigations) reached
;;;; through a tool and a slash command instead of a `record` subcommand.

(defsystem "nodecode-workflow-record"
  :description "Record a browser workflow over CDP, then write it as a replayable skill"
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-workflow-record/test"))))

(defsystem "nodecode-workflow-record/test"
  :description "Recording tests. Registered into the shared nodecode.test registry; RUN-WORKFLOW-RECORD-TESTS filters by the WORKFLOW-RECORD-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-workflow-record" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-workflow-record-tests)))
