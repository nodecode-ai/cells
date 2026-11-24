;;;; nodecode-ux3.asd --- product judgment across user, evidence and business
;;;; flows, as the model's own verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ux3' section, registers the ux3_gate tool and /ux3, and puts
;;;; the method on (help :ux3).
;;;;
;;;; Ported from cis2042's ux3-product-design-harness (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-ux3"
  :description "Product judgment across user, evidence and business flows before execution"
  :license "MIT"
  :author "cis2042 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "kernel")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ux3/test"))))

(defsystem "nodecode-ux3/test"
  :description "UX3 tests. Registered into the shared nodecode.test registry; RUN-UX3-TESTS filters by the UX3-CELL- name prefix."
  :license "MIT"
  :author "cis2042 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ux3" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ux3-tests)))
