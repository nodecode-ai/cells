;;;; nodecode-lcm-x.asd --- LCM-X lossless context memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `lcm-x' section, registers lcm_search, lcm_expand,
;;;; lcm_recent and lcm_status, puts /lcm and (help :lcm-x) up, and hooks the
;;;; :frame point so every turn's input, answer and tool results are kept.
;;;; Every dependency is already in the serving image.

(defsystem "nodecode-lcm-x"
  :description "LCM-X lossless context memory: every turn kept verbatim, searchable and readable back"
  :author "electricsheephq (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-lcm-x/test"))))

(defsystem "nodecode-lcm-x/test"
  :description "LCM-X tests. Registered into the shared nodecode.test registry; RUN-LCM-X-TESTS filters by the LCM-X-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-lcm-x" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-lcm-x-tests)))
