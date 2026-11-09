;;;; nodecode-caprover.asd --- plan, approve and deploy one CapRover image.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of fmercurio's hermes-caprover-operator as a Nodecode cell; see
;;;; NOTICE. Five tools the model calls, one /caprover command the operator
;;;; types, and a ledger of JSON files under the home.

(defsystem "nodecode-caprover"
  :description "Plan, approve and deploy one immutable image to a CapRover app"
  :license "MIT"
  :author "fmercurio (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-caprover/test"))))

(defsystem "nodecode-caprover/test"
  :description "The cell's lifecycle, its five tools over a stubbed CapRover, and its refusals."
  :license "MIT"
  :depends-on ("nodecode-caprover" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-caprover-tests)))
