;;;; nodecode-file-tray.asd --- the paths this session read or wrote, in one list.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of aydnOktay's hermes-file-tray as a Nodecode peripheral:
;;;; two tools, one command, one manual topic, and an advice on the file
;;;; verbs that keeps the list.

(defsystem "nodecode-file-tray"
  :description "The paths this session read or wrote, in one list"
  :license "MIT"
  :version "0.1.0"
  :author "aydnOktay (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-file-tray/test"))))

(defsystem "nodecode-file-tray/test"
  :description "File tray tests, registered into the shared nodecode.test registry; RUN-FILE-TRAY-TESTS runs the FILE-TRAY-CELL- slice."
  :license "MIT"
  :depends-on ("nodecode-file-tray" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-file-tray-tests)))
