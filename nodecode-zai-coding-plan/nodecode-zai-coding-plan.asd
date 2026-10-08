;;;; nodecode-zai-coding-plan.asd --- Z.AI's GLM Coding Plan, signed in from the browser, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `zai-coding-plan' section and installs the hooks and the
;;;; command cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's zai-coding-plan sign-in and its zai provider (see
;;;; NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-zai-coding-plan"
  :description "Z.AI GLM Coding Plan, signed in from the browser, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-zai-coding-plan/test"))))

(defsystem "nodecode-zai-coding-plan/test"
  :description "Z.AI Coding Plan tests. Registered into the shared nodecode.test registry; RUN-ZAI-CODING-PLAN-TESTS filters by the ZAI-CODING-PLAN-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-zai-coding-plan" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-zai-coding-plan-tests)))
