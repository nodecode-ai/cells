;;;; nodecode-pocket-watch.asd --- what the scheduled jobs are doing, as a report.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes Agent plugin pocket-watch (see NOTICE). Optional
;;;; ASDF cell, not part of the organism core: the folder loader loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/, and the gateway
;;;; calls START-CELL after recovery, which reads the `pocket-watch' section,
;;;; registers the pocket_watch tool, /pocket-watch and (help :pocket-watch).

(defsystem "nodecode-pocket-watch"
  :description "Pocket watch: the scheduled jobs as a report, with a health score and the day, week and month grids"
  :license "MIT"
  :author "indigokarasu (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pocket-watch/test"))))

(defsystem "nodecode-pocket-watch/test"
  :description "Pocket watch tests. Registered into the shared nodecode.test registry; RUN-POCKET-WATCH-TESTS filters by the POCKET-WATCH-CELL- name prefix."
  :license "MIT"
  :author "indigokarasu (original), Nodecode (rewrite)"
  :depends-on ("nodecode-pocket-watch" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pocket-watch-tests)))
