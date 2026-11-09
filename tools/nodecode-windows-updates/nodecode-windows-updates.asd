;;;; nodecode-windows-updates.asd --- the Windows update history, read-only.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Yicheng-Mark's hermes-windows-plugins windows-updates folder:
;;;; two tools and one /windows-updates command over the Windows Update Agent's
;;;; session interface and CIM. Nothing is searched for, downloaded, installed
;;;; or uninstalled, and no file is written. Windows only.

(defsystem "nodecode-windows-updates"
  :description "The Windows update history and quick-fix list, read-only, as two tools"
  :license "MIT"
  :version "0.1.0"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-windows-updates/test"))))

(defsystem "nodecode-windows-updates/test"
  :description "windows-updates tests. Registered into the shared nodecode.test registry; RUN-WINDOWS-UPDATES-TESTS filters by the WINDOWS-UPDATES-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-windows-updates" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-windows-updates-tests)))
