;;;; nodecode-iwork.asd --- Apple Numbers, Keynote and Pages files, edited
;;;; through the apps themselves.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `iwork' section, registers the five tools, puts
;;;; the primer on (help :iwork) and /iwork in the slash catalog.
;;;;
;;;; Rewritten from Arkanji's Hermes Agent plugin iwork-studio; see NOTICE.
;;;; Every dependency rides with nodecode: the apps are driven through the
;;;; osascript that ships with macOS, and nothing else is asked for.

(defsystem "nodecode-iwork"
  :description "Apple Numbers, Keynote and Pages: read a document, set a cell, edit slides, export, undo"
  :author "Arkanji (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "jxa")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-iwork/test"))))

(defsystem "nodecode-iwork/test"
  :description "iWork tests. Registered into the shared nodecode.test registry; RUN-IWORK-TESTS filters by the IWORK-CELL- name prefix."
  :author "Arkanji (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-iwork" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-iwork-tests)))
