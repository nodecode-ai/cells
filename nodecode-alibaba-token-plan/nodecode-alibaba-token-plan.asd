;;;; nodecode-alibaba-token-plan.asd --- QwenCloud Token Plan as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `alibaba-token-plan' section and installs the hooks cell.lisp
;;;; lists.
;;;;
;;;; Ported from oh-my-pi's alibaba-token-plan provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-alibaba-token-plan"
  :description "QwenCloud Token Plan: Alibaba's regional token subscription as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-alibaba-token-plan/test"))))

(defsystem "nodecode-alibaba-token-plan/test"
  :description "QwenCloud Token Plan tests. Registered into the shared nodecode.test registry; RUN-ALIBABA-TOKEN-PLAN-TESTS filters by the ALIBABA-TOKEN-PLAN-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-alibaba-token-plan" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-alibaba-token-plan-tests)))
