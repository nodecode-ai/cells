;;;; nodecode-northstar.asd --- instruction files that disagree with each other.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. A Nodecode rewrite of mrcoxis-2002/hermes-northstar, whose
;;;; readiness check reads the files a session is told to obey instead of what
;;;; it is running. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `northstar' section, registers `northstar_audit', puts
;;;; /northstar in the slash catalog and the manual on (help :northstar).

(defsystem "nodecode-northstar"
  :description "Readiness check: instruction files that contradict, repeat or dangle"
  :license "MIT"
  :version "0.1.0"
  :author "mrcoxis-2002 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-northstar/test"))))

(defsystem "nodecode-northstar/test"
  :description "Northstar tests. Registered into the shared nodecode.test registry; RUN-NORTHSTAR-TESTS filters by the NORTHSTAR-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-northstar" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-northstar-tests)))
