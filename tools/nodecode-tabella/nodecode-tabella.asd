;;;; nodecode-tabella.asd --- documents read, converted and written, with pandoc.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `tabella' section, registers the three tools and puts the
;;;; primer on (help :tabella).
;;;;
;;;; Rewritten from dyiapanis's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: the programs a person
;;;; already has (pandoc, pdftotext, a LaTeX engine) are run as argv.

(defsystem "nodecode-tabella"
  :description "Tabella: read, convert and create office documents with pandoc"
  :author "dyiapanis (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "proc")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tabella/test"))))

(defsystem "nodecode-tabella/test"
  :description "Tabella tests. Registered into the shared nodecode.test registry; RUN-TABELLA-TESTS filters by the TABELLA-CELL- name prefix."
  :author "dyiapanis (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-tabella" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tabella-tests)))
