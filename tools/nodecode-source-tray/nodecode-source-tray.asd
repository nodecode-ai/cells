;;;; nodecode-source-tray.asd --- the URLs this session opened, in one list.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of aydnOktay's hermes-source-tray as a Nodecode peripheral:
;;;; two tools, one command, one manual topic, and an advice on the :tool
;;;; point that reads the URLs a watched call's answer carried.

(defsystem "nodecode-source-tray"
  :description "The URLs this session opened, in one list"
  :license "MIT"
  :version "0.1.0"
  :author "aydnOktay (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-source-tray/test"))))

(defsystem "nodecode-source-tray/test"
  :description "Source tray tests, registered into the shared nodecode.test registry; RUN-SOURCE-TRAY-TESTS runs the SOURCE-TRAY-CELL- slice."
  :license "MIT"
  :depends-on ("nodecode-source-tray" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-source-tray-tests)))
