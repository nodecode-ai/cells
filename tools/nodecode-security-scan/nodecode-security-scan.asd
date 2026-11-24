;;;; nodecode-security-scan.asd --- security reviews of a local repository.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Optional ASDF cell, NOT part of the organism core. The folder loader loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/; the gateway
;;;; calls START-CELL after recovery, which reads the `security-scan' section,
;;;; registers the nine security_scan_* tools in the security toolset, puts
;;;; /security in the slash catalog and the manual on (help :security-scan).
;;;;
;;;; A scan is one JSON file under the home; the reports are written beside
;;;; it when the scan is sealed. Every dependency is already in the serving
;;;; image.

(defsystem "nodecode-security-scan"
  :description "Security reviews of a local repository: nine tools, /security, and md/json/sarif/csv reports"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell")
               (:file "tools"))
  :in-order-to ((test-op (test-op "nodecode-security-scan/test"))))

(defsystem "nodecode-security-scan/test"
  :description "Security-scan tests. Registered into the shared nodecode.test registry; RUN-SECURITY-SCAN-TESTS filters by the SECSCAN-CELL- name prefix."
  :license "Apache-2.0"
  :depends-on ("nodecode-security-scan" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-security-scan-tests)))
