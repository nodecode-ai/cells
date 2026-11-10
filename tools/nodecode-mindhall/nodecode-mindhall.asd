;;;; nodecode-mindhall.asd --- one shared mind, four rooms, a signed change log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `mindhall' section, registers the five mindhall_* tools and
;;;; /mindhall, hooks the standing harness sections so every turn carries the
;;;; mind's short block, and puts the manual on (help :mindhall).
;;;;
;;;; A rewrite of jai-srinivas's Hermes plugin of the same purpose (see
;;;; NOTICE): the same four rooms, the same five tools and the same block in
;;;; front of a turn, kept in one JSON file instead of SQLite.

(defsystem "nodecode-mindhall"
  :description "One shared mind for the AIs you use: about you, work, dates and a signed change log"
  :license "MIT"
  :version "0.1.0"
  :author "jai-srinivas (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mindhall/test"))))

(defsystem "nodecode-mindhall/test"
  :description "Mindhall tests. Registered into the shared nodecode.test registry; RUN-MINDHALL-TESTS filters by the MINDHALL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-mindhall" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mindhall-tests)))
