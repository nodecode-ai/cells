;;;; nodecode-xiaomi.asd --- Xiaomi MiMo, pay-as-you-go or Token Plan, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `xiaomi' section and installs the hooks cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's xiaomi provider (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-xiaomi"
  :description "Xiaomi MiMo: its models, on a pay-as-you-go key or a regional Token Plan key"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-xiaomi/test"))))

(defsystem "nodecode-xiaomi/test"
  :description "Xiaomi tests. Registered into the shared nodecode.test registry; RUN-XIAOMI-TESTS filters by the XIAOMI-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-xiaomi" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-xiaomi-tests)))
