;;;; nodecode-secret-gate.asd --- a deterministic secret scanner before text leaves.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `secret-gate' section, advises the :TOOL point so every
;;;; tool result is audited, and registers the three tools, /secret-gate and
;;;; (help :secret-gate).
;;;;
;;;; The only dependency beyond nodecode is cl-ppcre, which the serving image
;;;; already carries; the scanner itself reads a string and touches nothing
;;;; else -- no network, no file, no subprocess.

(defsystem "nodecode-secret-gate"
  :description "Scans text for leaked credentials and gates outbound shares, fail-closed"
  :license "MIT"
  :author "MSAbhishek22 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "scan")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-secret-gate/test"))))

(defsystem "nodecode-secret-gate/test"
  :description "Secret-gate tests. Registered into the shared nodecode.test registry; RUN-SECRET-GATE-TESTS filters by the SECRET-GATE-CELL- name prefix."
  :license "MIT"
  :author "MSAbhishek22 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-secret-gate" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-secret-gate-tests)))
