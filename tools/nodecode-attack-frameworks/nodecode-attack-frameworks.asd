;;;; nodecode-attack-frameworks.asd --- five LLM-driven attack frameworks, and
;;;; what they leave in a log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The facts are the DFIR Radar post of 13 November 2026, read against each
;;;; repository's own GitHub metadata on 14 November 2026; the code is ours.
;;;; Nothing here redistributes a framework: two are AGPL-3.0, and a table about
;;;; a program is not a copy of it. See NOTICE.
;;;;
;;;; Three files beside this one: DATA.LISP, the table and the post's own
;;;; notes, QUERY.LISP, the filters and the blocks a person reads, and CELL.LISP,
;;;; the one declaration. The .asd sits INSIDE its folder: install by putting --
;;;; or symlinking -- this directory under ~/.nodecode/cells/.

(defsystem "nodecode-attack-frameworks"
  :description "LLM-driven attack frameworks already seen in real intrusions: what they are, and what they leave behind"
  :license "MIT"
  :author "DFIR Radar (the facts), Nodecode (this cell)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "query")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-attack-frameworks/test"))))

(defsystem "nodecode-attack-frameworks/test"
  :description "attack-frameworks tests. Registered into the shared nodecode.test registry; RUN-ATTACK-FRAMEWORKS-TESTS filters by the ATTACK-FRAMEWORKS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-attack-frameworks" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-attack-frameworks-tests)))
