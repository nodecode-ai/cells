;;;; nodecode-vibeleak.asd --- VibeLeak website assessments and a local source review.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `vibeleak' section, registers four tools, puts /vibeleak in the
;;;; catalog and the manual on (help :vibeleak).
;;;;
;;;; A Nodecode rewrite of abundantbeing's vibeleak-atlas; see NOTICE. No
;;;; dependency beyond nodecode itself: the local rules are Common Lisp over
;;;; the files a bounded walk reads, and the one network call is nlk:http to
;;;; the fixed VibeLeak origin.

(defsystem "nodecode-vibeleak"
  :description "VibeLeak: an owner-approved website assessment and a read-only local PHP/source review"
  :author "abundantbeing (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "rules")
               (:file "api")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-vibeleak/test"))))

(defsystem "nodecode-vibeleak/test"
  :description "VibeLeak tests. Registered into the shared nodecode.test registry; RUN-VIBELEAK-TESTS runs the VIBELEAK-CELL- slice."
  :author "abundantbeing (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-vibeleak" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-vibeleak-tests)))
