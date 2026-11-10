;;;; nodecode-on-brand.asd --- brand voice and compliance rules, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot while its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `on-brand' section, registers the four onbrand_* tools
;;;; and /on-brand, puts (help :on-brand) on the manual, and hooks the :TOOL
;;;; point and NLE::READ-HARNESS-SECTIONS.
;;;;
;;;; A Nodecode rewrite of Mhiah/On-Brand (see NOTICE). Every dependency is
;;;; already in the serving image (cl-ppcre rides with nodecode).

(defsystem "nodecode-on-brand"
  :description "Brand voice and compliance rules a deterministic linter enforces"
  :license "MIT"
  :version "0.1.0"
  :author "Mhiah (original), Nodecode (rewrite)"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "rules")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-on-brand/test"))))

(defsystem "nodecode-on-brand/test"
  :description "On-Brand tests. Registered into the shared nodecode.test registry; RUN-ON-BRAND-TESTS filters by the ON-BRAND-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Mhiah (original), Nodecode (rewrite)"
  :depends-on ("nodecode-on-brand" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-on-brand-tests)))
