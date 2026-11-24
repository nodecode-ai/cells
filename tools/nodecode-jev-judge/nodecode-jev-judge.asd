;;;; nodecode-jev-judge.asd --- one Jev judgment before a call runs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `jev-judge' section, hooks the gate onto the core's :TOOL point,
;;;; registers /jev-judge and puts the manual on (help :jev-judge).
;;;;
;;;; Rewritten from DoGMaTiiC's Hermes Agent plugin jev-judge; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to the judge and
;;;; cl-ppcre reads the redaction patterns, as it does for the shipped cells.

(defsystem "nodecode-jev-judge"
  :description "Jev judge: one Jev judgment gates a call before it runs (shadow or enforce)"
  :author "DoGMaTiiC (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jev-judge/test"))))

(defsystem "nodecode-jev-judge/test"
  :description "Jev judge tests. Registered into the shared nodecode.test registry; RUN-JEV-JUDGE-TESTS filters by the JEV-JUDGE-CELL- name prefix."
  :author "DoGMaTiiC (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-jev-judge" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jev-judge-tests)))
