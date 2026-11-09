;;;; nodecode-doctorpack.asd --- local diagnostics, safety audit and incident postmortems.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `doctorpack' section, registers three tools,
;;;; /doctorpack and the manual topic (help :doctorpack).
;;;;
;;;; Nothing in it leaves this machine: it reads the home's config.jsonc, its
;;;; cells folder and its logs, and writes redacted markdown reports under
;;;; <home>/doctorpack/reports/. Every dependency is already in the serving
;;;; image (cl-ppcre, shasht).

(defsystem "nodecode-doctorpack"
  :description "Local diagnostics, safety audit and redacted incident postmortems"
  :license "MIT"
  :author "godhiraj-code (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "audit")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-doctorpack/test"))))

(defsystem "nodecode-doctorpack/test"
  :description "Doctorpack tests. Registered into the shared nodecode.test registry; RUN-DOCTORPACK-TESTS filters by the DOCTORPACK-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-doctorpack" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-doctorpack-tests)))
