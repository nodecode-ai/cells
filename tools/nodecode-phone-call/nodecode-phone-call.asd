;;;; nodecode-phone-call.asd --- Phone Call: approved outbound calls with an AI disclosure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `phone-call' section, registers the four tools cell.lisp lists
;;;; and puts the plugin's rules on the manual as (help :phone-call).
;;;;
;;;; A rewrite of winchxyz's hermes-phone (see NOTICE): the original carried a
;;;; realtime voice session over Twilio Media Streams from a FastAPI bridge,
;;;; and asked the operator mid-call for what the brief did not cover. This
;;;; cell keeps the brief, the approval, the blocklist, the allowlist, the
;;;; consent model and the limits, and dials a scripted TwiML message instead.

(defsystem "nodecode-phone-call"
  :description "Phone Call: an approved outbound call with an AI disclosure, and what came of it"
  :author "winchxyz (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "safety")
               (:file "brief")
               (:file "wire")
               (:file "store")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-phone-call/test"))))

(defsystem "nodecode-phone-call/test"
  :description "Phone Call cell tests. Registered into the shared nodecode.test registry; RUN-PHONE-CALL-TESTS filters by the PHONE-CALL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-phone-call" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-phone-call-tests)))
