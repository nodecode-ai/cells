;;;; nodecode-time-machine.asd --- restore points, and what a restore cannot undo.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `time-machine' section, hooks the :TOOL point so a call
;;;; that changes files earns a restore point and a call that crosses a point
;;;; of no return is written into the effect ledger, registers /time-machine
;;;; and puts the manual on (help :time-machine). Every dependency is already
;;;; in the serving image.

(defsystem "nodecode-time-machine"
  :description "Restore points for a folder, a rewind that previews, and the effects a rewind cannot undo"
  :license "MIT"
  :version "0.1.0"
  :author "ddaikodaiko (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-time-machine/test"))))

(defsystem "nodecode-time-machine/test"
  :description "Time Machine tests. Registered into the shared nodecode.test registry; RUN-TIME-MACHINE-TESTS filters by the TIME-MACHINE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-time-machine" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-time-machine-tests)))
