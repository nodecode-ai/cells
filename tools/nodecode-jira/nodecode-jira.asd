;;;; nodecode-jira.asd --- Jira's read-only tools as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `jira' section, registers the four tools and the /jira command,
;;;; and puts the read-only rule on the manual as (help :jira).
;;;;
;;;; Rewritten from the Jira Browser Hermes plugin; see NOTICE. The original
;;;; was a Desktop board over a Python service; this cell speaks Jira Cloud's
;;;; REST API itself, through nlk:http.

(defsystem "nodecode-jira"
  :description "Jira: assigned issues, bounded JQL search, issue detail and available transitions, read-only"
  :author "LikelyLucid (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jira/test"))))

(defsystem "nodecode-jira/test"
  :description "Jira cell tests. Registered into the shared nodecode.test registry; RUN-JIRA-TESTS filters by the JIRA-CELL- name prefix."
  :author "LikelyLucid (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-jira" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jira-tests)))
