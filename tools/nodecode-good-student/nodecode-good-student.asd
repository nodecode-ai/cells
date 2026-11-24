;;;; nodecode-good-student.asd --- wrong questions a parent confirms, weak
;;;; points measured by independent reassessments, a week of small actions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `good-student' section, registers twelve good_student_* tools
;;;; and /good-student, and puts (help :good-student) on the manual.
;;;;
;;;; A Nodecode rewrite of harrylabsj/hermes-plugin-good-student (see NOTICE).

(defsystem "nodecode-good-student"
  :description "Good Student: wrong questions a parent confirms, analysed into weak points and a week of study actions"
  :license "MIT"
  :version "0.1.0"
  :author "harrylabsj (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-good-student/test"))))

(defsystem "nodecode-good-student/test"
  :description "Good Student tests. Registered into the shared nodecode.test registry; RUN-GOOD-STUDENT-TESTS filters by the GOOD-STUDENT-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "harrylabsj (original), Nodecode (rewrite)"
  :depends-on ("nodecode-good-student" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-good-student-tests)))
