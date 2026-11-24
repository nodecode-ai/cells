;;;; nodecode-fluent.asd --- the agent's terminal commands checked against each program's help.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `fluent' section, puts (help :fluent) on the manual, registers
;;;; fluent_check and /fluent, and hooks one check onto the :TOOL point.
;;;;
;;;; A rewrite of Bruno-coder21/fluent (see NOTICE). Every dependency rides
;;;; with nodecode.

(defsystem "nodecode-fluent"
  :description "fluent: every terminal command checked against the program's own --help before it runs"
  :license "MIT"
  :version "0.1.0"
  :author "Bruno-coder21 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "help")
               (:file "adopt")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fluent/test"))))

(defsystem "nodecode-fluent/test"
  :description "fluent tests. Registered into the shared nodecode.test registry; RUN-FLUENT-TESTS filters by the FLUENT-CELL- name prefix."
  :license "MIT"
  :author "Bruno-coder21 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fluent" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fluent-tests)))
