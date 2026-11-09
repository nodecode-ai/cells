;;;; nodecode-storm.asd --- a perspective panel of models, then a judge.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `storm' section and registers the storm_run_panel tool, /storm
;;;; and (help :storm).
;;;;
;;;; A Nodecode rewrite of bmtrnavsky's storm-fusion-research-pipeline (see
;;;; NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-storm"
  :description "Storm Panel Research: one question across several model seats, then a judge synthesizes"
  :license "MIT"
  :author "bmtrnavsky (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-storm/test"))))

(defsystem "nodecode-storm/test"
  :description "Storm panel tests. Registered into the shared nodecode.test registry; RUN-STORM-TESTS filters by the STORM-CELL- name prefix."
  :license "MIT"
  :author "bmtrnavsky (original), Nodecode (rewrite)"
  :depends-on ("nodecode-storm" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-storm-tests)))
