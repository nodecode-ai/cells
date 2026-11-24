;;;; nodecode-triplescan.asd --- one folder, three local scanners, one verdict.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `triplescan' section, registers the triplescan tool and the
;;;; /triplescan command, puts the standing paragraph in every request, and the
;;;; manual on (help :triplescan).
;;;;
;;;; The three programs are the operator's own: Gitleaks, OSV-Scanner and
;;;; Semgrep CE, found on PATH or named in the section. See NOTICE: this folder
;;;; is a rewrite of dafka007/hermes-security-audit's Hermes plugin.

(defsystem "nodecode-triplescan"
  :description "Triplescan: Gitleaks, OSV-Scanner and Semgrep over one folder, with an honest verdict"
  :license "MIT"
  :version "0.1.0"
  :author "dafka007 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-triplescan/test"))))

(defsystem "nodecode-triplescan/test"
  :description "triplescan tests. Registered into the shared nodecode.test registry; RUN-TRIPLESCAN-TESTS filters by the TRIPLESCAN-CELL- name prefix."
  :license "MIT"
  :author "dafka007 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-triplescan" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-triplescan-tests)))
