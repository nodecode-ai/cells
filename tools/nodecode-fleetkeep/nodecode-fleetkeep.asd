;;;; nodecode-fleetkeep.asd --- review a vehicle CSV for a Fleetkeep import.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in src/ names this system.
;;;; The folder loader loads it at boot when this directory sits under
;;;; ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://fleetkeep' is accepted. No key, no network, no shell:
;;;; the review is arithmetic over the text it is handed.
;;;;
;;;; A Nodecode rewrite of jlucwright/fleetkeep-agent-plugin, a skills-only
;;;; package that carried one SKILL.md. See NOTICE.

(defsystem "nodecode-fleetkeep"
  :description "Review a vehicle CSV for missing, duplicate or ambiguous import values and write a Fleetkeep-ready file"
  :license "MIT"
  :author "jlucwright (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "fleetkeep")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fleetkeep/test"))))

(defsystem "nodecode-fleetkeep/test"
  :description "Fleetkeep tests. Registered into the shared nodecode.test registry; RUN-FLEETKEEP-TESTS filters by the FLEETKEEP- name prefix."
  :license "MIT"
  :depends-on ("nodecode-fleetkeep" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "fleetkeep-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fleetkeep-tests)))
