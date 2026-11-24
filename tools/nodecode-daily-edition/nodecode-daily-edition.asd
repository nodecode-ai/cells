;;;; nodecode-daily-edition.asd --- a dated front page from interest phrases.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `daily-edition' section and puts the daily_edition
;;;; tool, /daily-edition and (help :daily-edition) in reach.

(defsystem "nodecode-daily-edition"
  :description "A dated front page compiled from interest phrases: one story once, cross-posts merged, reprints withheld"
  :license "MIT"
  :author "ousiaresearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "edition")
               (:file "news")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-daily-edition/test"))))

(defsystem "nodecode-daily-edition/test"
  :description "Daily edition tests. Registered into the shared nodecode.test registry; RUN-DAILY-EDITION-TESTS filters by the DAILY-EDITION-CELL- name prefix."
  :license "MIT"
  :author "ousiaresearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode-daily-edition" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-daily-edition-tests)))
