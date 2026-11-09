;;;; nodecode-windows-apps.asd --- what is installed on a Windows machine, from the registry.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `windows-apps' section, registers the two tools and the
;;;; /windows-apps command, and puts the manual on (help :windows-apps).
;;;;
;;;; Rewritten from Yicheng-Mark's Hermes Agent plugin of the same name; see
;;;; NOTICE. The original read the four Uninstall keys through CPython's
;;;; winreg; this cell reads them through reg.exe, the registry tool Windows
;;;; ships, which prints the same values and needs no Python. Every other
;;;; dependency rides with Nodecode: nlk:run-bounded starts reg and bounds it,
;;;; nlk:define-contract types each tool's arguments.

(defsystem "nodecode-windows-apps"
  :description "Windows Apps: what is installed on this Windows machine, from the four Uninstall registry keys"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-windows-apps/test"))))

(defsystem "nodecode-windows-apps/test"
  :description "Windows Apps tests. Registered into the shared nodecode.test registry; RUN-WINDOWS-APPS-TESTS filters by the WINDOWS-APPS-CELL- name prefix."
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-windows-apps" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-windows-apps-tests)))
