;;;; nodecode-cline-pass.asd --- ClinePass, Cline's model subscription, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cline-pass' section and installs the hooks cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's cline-pass provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-cline-pass"
  :description "ClinePass: Cline's model subscription as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cline-pass/test"))))

(defsystem "nodecode-cline-pass/test"
  :description "ClinePass tests. Registered into the shared nodecode.test registry; RUN-CLINE-PASS-TESTS filters by the CLINE-PASS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cline-pass" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cline-pass-tests)))
