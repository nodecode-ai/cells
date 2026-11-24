;;;; nodecode-phone-harness.asd --- phone-harness as one Nodecode tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One peripheral: the `phone-harness' section, the PHONE_EXEC tool, the
;;;; /phone command and the manual topic. It speaks to the phone-harness
;;;; runner the operator already has, on PATH or named by the section's
;;;; command, so nothing here is bundled and no Python is installed by it.

(defsystem "nodecode-phone-harness"
  :description "Drive a real iPhone, Android or cloud phone from one phone_exec tool: taps, typing, screen reads and screenshots"
  :license "MIT"
  :author "phone-harness (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-phone-harness/test"))))

(defsystem "nodecode-phone-harness/test"
  :description "The phone-harness cell's tests, over a stubbed runner."
  :license "MIT"
  :author "phone-harness (original), Nodecode (rewrite)"
  :depends-on ("nodecode-phone-harness" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-phone-harness-tests)))
