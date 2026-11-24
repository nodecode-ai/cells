;;;; nodecode-phone-autopilot.asd --- phone-autopilot as one Nodecode tool: routine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of phone-autopilot's Hermes plugin. The original saved a
;;;; model-written Python routine in a folder of its own, bound the operator's
;;;; approval to the sha256 of the exact bytes, prepended an app guard to the
;;;; script, piped it to `python -m phone_harness.run' and turned the
;;;; `@@AUTOPILOT' events it printed into a run record: passed or failed, the
;;;; checkpoint that missed, the screenshot it left, the tail of what it
;;;; printed. This folder keeps that engine and runs the same script through
;;;; the phone-harness runner the operator already has, the way the
;;;; phone-harness cell beside it does. What the original had as a live phone
;;;; pane, image refs and a scheduled blueprint is left out; see NOTICE.

(defsystem "nodecode-phone-autopilot"
  :description "Save a phone task once as an approved script, then replay it on the phone with no model in the loop"
  :license "MIT"
  :author "jupist (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "ironclad/digest/sha256")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-phone-autopilot/test"))))

(defsystem "nodecode-phone-autopilot/test"
  :description "The phone-autopilot cell's tests, over a stubbed runner and a scratch folder."
  :license "MIT"
  :author "jupist (original), Nodecode (rewrite)"
  :depends-on ("nodecode-phone-autopilot" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-phone-autopilot-tests)))
