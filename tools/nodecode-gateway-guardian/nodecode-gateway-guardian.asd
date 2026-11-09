;;;; nodecode-gateway-guardian.asd --- the gateway's health in one line.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `gateway-guardian' section, registers the two tools and
;;;; /gateway-guardian, and puts the states and the recovery checklist on
;;;; (help :gateway-guardian).
;;;;
;;;; A rewrite of kawsthingz/kawsthingz-hermes-gateway-guardian (MIT); see
;;;; NOTICE. Every dependency is already in the serving image.

(defsystem "nodecode-gateway-guardian"
  :description "Gateway health in one line: OK, DEGRADED or BLOCKED, with the fault counts behind it"
  :license "MIT"
  :author "kawsthingz (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gateway-guardian/test"))))

(defsystem "nodecode-gateway-guardian/test"
  :description "Gateway Guardian tests. Registered into the shared nodecode.test registry; RUN-GATEWAY-GUARDIAN-TESTS filters by the GATEWAY-GUARDIAN-CELL- name prefix."
  :license "MIT"
  :author "kawsthingz (original), Nodecode (rewrite)"
  :depends-on ("nodecode-gateway-guardian" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gateway-guardian-tests)))
