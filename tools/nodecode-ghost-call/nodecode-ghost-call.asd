;;;; nodecode-ghost-call.asd --- Ghost Call: a silent coach on the operator's own phone call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ghost-call' section, registers the seven tools cell.lisp lists
;;;; and puts the plugin's coaching rules on the manual as (help :ghost).
;;;;
;;;; A rewrite of winchxyz's hermes-ghost-call (see NOTICE): the original kept
;;;; the operator on the line and had a FastAPI bridge transcribe both legs,
;;;; gate the transcript and ask a fast model for one line at a time. This cell
;;;; keeps the gate, the prompt, the rate limits and the summary, and takes the
;;;; lines the model or the operator types instead of a microphone.

(defsystem "nodecode-ghost-call"
  :description "Ghost Call: one-line hints while the operator is on a call, and a post-call summary"
  :author "winchxyz (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "hints")
               (:file "session")
               (:file "summary")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ghost-call/test"))))

(defsystem "nodecode-ghost-call/test"
  :description "Ghost Call cell tests. Registered into the shared nodecode.test registry; RUN-GHOST-CALL-TESTS filters by the GHOST-CALL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-ghost-call" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ghost-call-tests)))
