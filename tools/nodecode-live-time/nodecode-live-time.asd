;;;; nodecode-live-time.asd --- the real current time in every turn's context.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery. It is
;;;; a rewrite of a Hermes Agent plugin -- see NOTICE.

(defsystem "nodecode-live-time"
  :description "The real current time in every turn's standing context, so today and now never follow a stale session stamp"
  :license "MIT"
  :author "chenfeijiang95-ui (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-live-time/test"))))

(defsystem "nodecode-live-time/test"
  :description "Tests for the live-time cell; RUN-LIVE-TIME-TESTS filters the shared registry by the LIVE-TIME- prefix."
  :license "MIT"
  :author "chenfeijiang95-ui (original), Nodecode (rewrite)"
  :depends-on ("nodecode-live-time" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-live-time-tests)))
