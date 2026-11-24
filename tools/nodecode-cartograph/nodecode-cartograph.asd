;;;; nodecode-cartograph.asd --- a local repository mapped, and the source a task needs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cartograph' section, puts (help :cartograph) on the manual,
;;;; registers the /cartograph command and the two tools cell.lisp declares.
;;;;
;;;; Ported from the Hermes plugin anthony-maio/cartograph ships
;;;; (assets/hermes); see NOTICE.

(defsystem "nodecode-cartograph"
  :description "A local repository mapped, and the source excerpts a task needs: static, no key"
  :license "MIT"
  :author "anthony-maio (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cartograph/test"))))

(defsystem "nodecode-cartograph/test"
  :description "Cartograph tests. Registered into the shared nodecode.test registry; RUN-CARTOGRAPH-TESTS filters by the CARTOGRAPH-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cartograph" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cartograph-tests)))
