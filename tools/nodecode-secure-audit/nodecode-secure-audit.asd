;;;; nodecode-secure-audit.asd --- read-only security and compliance tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `secure-audit' section and registers the three tools,
;;;; /secure-audit and (help :secure-audit).
;;;;
;;;; Nothing here leaves the machine: the rule engine, the redactor and the
;;;; audit are local string and file work.

(defsystem "nodecode-secure-audit"
  :description "Prompt-injection scan, Chinese-PII redaction and a local configuration audit"
  :license "MIT"
  :author "PensiveFei (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "rules")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-secure-audit/test"))))

(defsystem "nodecode-secure-audit/test"
  :description "Tests for the secure-audit cell. Registered into the shared nodecode.test registry; RUN-SECURE-AUDIT-TESTS filters by the SECURE-AUDIT-CELL- name prefix."
  :license "MIT"
  :author "PensiveFei (original), Nodecode (rewrite)"
  :depends-on ("nodecode-secure-audit" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-secure-audit-tests)))
