;;;; nodecode-refusal.asd --- reads every answer; asks again when it declines.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of 0ldev/refusal (a Hermes Agent plugin) as a Nodecode cell:
;;;; the same detection over an answer's prose, on the two seams Nodecode
;;;; already has -- a :FRAME hook for the turn that just ended, a :ROUND hook
;;;; for the request about to be asked. No dependencies beyond nodecode.

(defsystem "nodecode-refusal"
  :description "Retries an answer that declined the request, with your own preprompt"
  :license "MIT"
  :version "0.1.0"
  :author "0ldev (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "detect")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-refusal/test"))))

(defsystem "nodecode-refusal/test"
  :description "Refusal tests: the detector, the two hooks, the command and the tool."
  :license "MIT"
  :author "0ldev (original), Nodecode (rewrite)"
  :depends-on ("nodecode-refusal" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-refusal-tests)))
