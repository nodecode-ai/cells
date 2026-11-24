;;;; nodecode-undo.asd --- the files a write, an edit or an rm removes, kept.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: the folder loader
;;;; loads it at boot when it sits under ~/.nodecode/cells/, and the gateway
;;;; calls START-CELL after recovery, which reads the `undo' section, hooks
;;;; WRITE-FILE, EDIT and SH, registers /undo and puts the manual on
;;;; (help :undo). Every dependency is already in the serving image.

(defsystem "nodecode-undo"
  :description "Put back the files an agent deleted or overwrote"
  :license "MIT"
  :version "0.1.0"
  :author "Moodtuner997 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-undo/test"))))

(defsystem "nodecode-undo/test"
  :description "Undo tests. Registered into the shared nodecode.test registry; RUN-UNDO-TESTS filters by the UNDO-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-undo" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-undo-tests)))
