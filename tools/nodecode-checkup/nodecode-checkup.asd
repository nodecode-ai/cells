;;;; nodecode-checkup.asd --- the half of a health check that /doctor leaves out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. A Nodecode rewrite of iap/hermes-guide, whose self-check
;;;; complements the harness's own doctor instead of replacing it. The folder
;;;; loader loads it at boot when its directory sits under ~/.nodecode/cells/;
;;;; the gateway calls START-CELL after recovery, which reads the `checkup'
;;;; section, registers /checkup and run_checkup, and puts (help :checkup) on
;;;; the manual.

(defsystem "nodecode-checkup"
  :description "What /doctor leaves out: half-filled sections, refused cells, standing notices"
  :license "MIT"
  :version "0.1.0"
  :author "iap (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-checkup/test"))))

(defsystem "nodecode-checkup/test"
  :description "Checkup tests. Registered into the shared nodecode.test registry; RUN-CHECKUP-TESTS filters by the CHECKUP-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-checkup" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-checkup-tests)))
