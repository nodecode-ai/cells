;;;; nodecode-tabular.asd --- the tabular skill pack, with the part a machine
;;;; can do done here.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `tabular' section, puts (help :tabular) on the manual, and
;;;; registers the three tools and /tabular cell.lisp declares.
;;;;
;;;; A rewrite of frankiethull/hermes-tabular (see NOTICE): its eleven skills
;;;; are prose in skill/, unchanged, and the two steps that need no model --
;;;; profiling a file and splitting it -- are Lisp here. Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-tabular"
  :description "Tabular: profile and split a delimited file, and the skill pack a tabular analysis starts from"
  :license "MIT"
  :author "frankiethull (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "table")
               (:file "profile")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tabular/test"))))

(defsystem "nodecode-tabular/test"
  :description "Tabular tests. Registered into the shared nodecode.test registry; RUN-TABULAR-TESTS filters by the TABULAR-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-tabular" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tabular-tests)))
