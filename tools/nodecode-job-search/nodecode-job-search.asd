;;;; nodecode-job-search.asd --- live job postings, judged in words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `job-search' section, registers the two tools and the
;;;; /jobsearch command, and puts the manual on (help :job-search).
;;;;
;;;; The one thing outside this folder is the agent-data program the user
;;;; installs (npm install -g agent-data): it searches four job sources and
;;;; reads one posting in full. See NOTICE: this folder is a rewrite of
;;;; agent-data/job-search's Hermes plugin.

(defsystem "nodecode-job-search"
  :description "Job Search: live job postings, judged against your own prose preferences"
  :license "MIT"
  :version "0.1.0"
  :author "agent-data (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-job-search/test"))))

(defsystem "nodecode-job-search/test"
  :description "Job Search tests. Registered into the shared nodecode.test registry; RUN-JOB-SEARCH-TESTS filters by the JOB-SEARCH-CELL- name prefix."
  :license "MIT"
  :author "agent-data (original), Nodecode (rewrite)"
  :depends-on ("nodecode-job-search" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-job-search-tests)))
