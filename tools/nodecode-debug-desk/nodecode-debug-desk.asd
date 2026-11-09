;;;; nodecode-debug-desk.asd --- today's git digest and the last failed shell call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `debug-desk' section, registers the two tools and the
;;;; /debug-desk command, hooks the :TOOL point so a failed shell call is
;;;; recorded, and puts the manual on (help :debug-desk).
;;;;
;;;; See NOTICE: this folder is a rewrite of aydnOktay/hermes-debug-desk's
;;;; Hermes plugin.

(defsystem "nodecode-debug-desk"
  :description "Today's git digest and the last failed shell command the agent ran"
  :license "MIT"
  :version "0.1.0"
  :author "aydnOktay (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-debug-desk/test"))))

(defsystem "nodecode-debug-desk/test"
  :description "debug-desk tests. Registered into the shared nodecode.test registry; RUN-DEBUG-DESK-TESTS filters by the DEBUG-DESK-CELL- name prefix."
  :license "MIT"
  :author "aydnOktay (original), Nodecode (rewrite)"
  :depends-on ("nodecode-debug-desk" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-debug-desk-tests)))
