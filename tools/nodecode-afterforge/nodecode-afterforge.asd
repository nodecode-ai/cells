;;;; nodecode-afterforge.asd --- reviewed regression checks from what went wrong.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `afterforge' section, registers the six tools, the /afterforge
;;;; command and the manual on (help :afterforge).
;;;;
;;;; The whole point of the original -- a failure a person reviewed becomes a
;;;; check that runs again -- is kept; its storage is this cell's own folder
;;;; of JSON files under the organism's home. See NOTICE: this folder is a
;;;; rewrite of ussyverse/afterforge's Hermes plugin.

(defsystem "nodecode-afterforge"
  :description "Reviewed regression checks: a failure or correction, a command that proves it fixed"
  :license "MIT"
  :version "0.1.0"
  :author "ussyverse (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-afterforge/test"))))

(defsystem "nodecode-afterforge/test"
  :description "afterforge tests. Registered into the shared nodecode.test registry; RUN-AFTERFORGE-TESTS filters by the AFTERFORGE-CELL- name prefix."
  :license "MIT"
  :author "ussyverse (original), Nodecode (rewrite)"
  :depends-on ("nodecode-afterforge" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-afterforge-tests)))
