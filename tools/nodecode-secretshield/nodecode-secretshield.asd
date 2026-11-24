;;;; nodecode-secretshield.asd --- credentials stop at the tool boundary.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `secretshield' section, hooks the core's :TOOL point (a
;;;; credential in a call's arguments never reaches the tool that would send
;;;; it out), states one note in each session's first round, and registers
;;;; the shield_status tool, /secretshield and (help :secretshield).
;;;;
;;;; Every dependency is already in the serving image (cl-ppcre rides with
;;;; nodecode). The .asd sits INSIDE its folder: install by putting -- or
;;;; symlinking -- the directory under ~/.nodecode/cells/.

(defsystem "nodecode-secretshield"
  :description "Stops credentials from passing through tool calls, and keeps a local incident ledger"
  :license "MIT"
  :author "Whydiriki700 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "scanner")
               (:file "ledger")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-secretshield/test"))))

(defsystem "nodecode-secretshield/test"
  :description "SecretShield tests. Registered into the shared nodecode.test registry; RUN-SECRETSHIELD-TESTS filters by the SECRETSHIELD-CELL- name prefix."
  :license "MIT"
  :author "Whydiriki700 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-secretshield" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-secretshield-tests)))
