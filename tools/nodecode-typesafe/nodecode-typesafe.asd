;;;; nodecode-typesafe.asd --- TypeSafe's System One judgments, as functions the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `typesafe' section, puts (help :typesafe) on the manual and
;;;; registers /typesafe.
;;;;
;;;; Ported from oh-my-pi's typesafe provider (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-typesafe"
  :description "TypeSafe: typed judgments over a state (System One), called through eval"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "judge")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-typesafe/test"))))

(defsystem "nodecode-typesafe/test"
  :description "TypeSafe tests. Registered into the shared nodecode.test registry; RUN-TYPESAFE-TESTS filters by the TYPESAFE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-typesafe" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-typesafe-tests)))
