;;;; nodecode-office.asd --- guarded Word, PowerPoint and Excel edits.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `office' section, registers the five tools, puts the primer on
;;;; (help :office) and /office in the slash catalog.
;;;;
;;;; Rewritten from DECRUX9812's Hermes Agent plugin office-desk; see NOTICE.
;;;; Every dependency rides with nodecode: chipz inflates the ZIP entries,
;;;; and nothing else is asked for - no Office tool, no LibreOffice, no
;;;; network.

(defsystem "nodecode-office"
  :description "Guarded Word, PowerPoint and Excel edits: inspect, search, replace, tracked changes, receipts"
  :author "DECRUX9812 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "zip")
               (:file "office")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-office/test"))))

(defsystem "nodecode-office/test"
  :description "Office tests. Registered into the shared nodecode.test registry; RUN-OFFICE-TESTS filters by the OFFICE-CELL- name prefix."
  :author "DECRUX9812 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-office" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-office-tests)))
