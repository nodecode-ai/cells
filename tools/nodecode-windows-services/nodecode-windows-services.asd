;;;; nodecode-windows-services.asd --- Windows services as two tools: what runs,
;;;; what failed, and what depends on what.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `windows-services' section, registers the two tools cell.lisp
;;;; declares and puts the plugin's guide on the manual as
;;;; (help :windows-services).
;;;;
;;;; A rewrite of Yicheng-Mark's hermes-windows-services (see NOTICE): the
;;;; plugin ran one PowerShell CIM query and read the registry through
;;;; Python's winreg. This cell runs the same queries through PowerShell
;;;; alone and decodes the answers in Lisp, so it needs no Python, no Hermes
;;;; runtime and no manifest. Every dependency rides with nodecode; the
;;;; PowerShell it drives is Windows' own.

(defsystem "nodecode-windows-services"
  :description "Windows services: what runs, why one failed, and what depends on what, as two tools"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "powershell")
               (:file "meaning")
               (:file "services")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-windows-services/test"))))

(defsystem "nodecode-windows-services/test"
  :description "Windows services tests. Registered into the shared nodecode.test registry; RUN-WINDOWS-SERVICES-TESTS filters by the WINDOWS-SERVICES-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-windows-services" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-windows-services-tests)))
