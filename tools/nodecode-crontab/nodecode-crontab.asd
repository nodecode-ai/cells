;;;; nodecode-crontab.asd --- what a cron expression fires, in words and in dates.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery. It is
;;;; a rewrite of a Hermes Agent plugin -- see NOTICE.

(defsystem "nodecode-crontab"
  :description "Explains a cron expression in words, field by field, with the runs it fires next in a timezone of your choosing"
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-crontab/test"))))

(defsystem "nodecode-crontab/test"
  :description "Tests for the crontab cell; RUN-CRONTAB-TESTS filters the shared registry by the CRONTAB- prefix."
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-crontab" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-crontab-tests)))
