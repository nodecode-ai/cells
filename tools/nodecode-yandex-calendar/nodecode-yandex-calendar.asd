;;;; nodecode-yandex-calendar.asd --- Yandex Calendar over CalDAV, as seven tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of akinfold/hermes-yandex-calendar. The folder contract is in
;;;; Nodecode's src/CELLS.md; the reading, the writing and the client are
;;;; plain Common Lisp under cell.lisp, and only caldav.lisp reaches the
;;;; network (NLK:HTTP).

(defsystem "nodecode-yandex-calendar"
  :description "Yandex Calendar over CalDAV: list, create, update, answer, move and delete events"
  :license "MIT"
  :version "0.1.0"
  :author "akinfold (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "xml")
               (:file "ical")
               (:file "caldav")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-yandex-calendar/test"))))

(defsystem "nodecode-yandex-calendar/test"
  :description "The cell's tests: its lifecycle, its seven tools over a stubbed CalDAV server."
  :license "MIT"
  :depends-on ("nodecode-yandex-calendar" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-yandex-calendar-tests)))
