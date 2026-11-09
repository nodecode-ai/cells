;;;; nodecode-staff.asd --- Staff: a roster of specialists, a job board, and a
;;;; handoff to a coworker outside this process.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `staff' section, registers the five staff_* tools, /staff and the
;;;; manual topic (help :staff), and makes the folder the board is kept in.
;;;;
;;;; Rewritten from leoresendedop-sys's Hermes Agent plugin of the same name;
;;;; see NOTICE. Every dependency rides with nodecode: the specialists answer
;;;; through one NLK:COMPLETE side call, the handoff is one NLK:HTTP POST or one
;;;; file, and nothing else leaves the process.

(defsystem "nodecode-staff"
  :description "Staff: a roster of specialists the model assigns one-shot briefs to, a job board, and an optional handoff to a coworker over a webhook or a shared folder"
  :author "leoresendedop-sys (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "specialists")
               (:file "store")
               (:file "bridge")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-staff/test"))))

(defsystem "nodecode-staff/test"
  :description "Staff tests. Registered into the shared nodecode.test registry; RUN-STAFF-TESTS filters by the STAFF-CELL- name prefix."
  :author "leoresendedop-sys (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-staff" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-staff-tests)))
