;;;; nodecode-upstash.asd --- Upstash Redis memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `upstash' section, registers the four tools and /upstash, and
;;;; hooks the frame point that recalls memories into the prompt and saves
;;;; each finished turn's transcript.
;;;;
;;;; A Nodecode rewrite of upstash/agentkit's Hermes memory plugin (see NOTICE).

(defsystem "nodecode-upstash"
  :description "Upstash Redis memory: durable facts and searchable chat transcripts in your own Redis database"
  :license "MIT"
  :version "0.1.0"
  :author "upstash (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-upstash/test"))))

(defsystem "nodecode-upstash/test"
  :description "Upstash memory tests. Registered into the shared nodecode.test registry; RUN-UPSTASH-TESTS filters by the UPSTASH-CELL- name prefix."
  :license "MIT"
  :author "upstash (original), Nodecode (rewrite)"
  :depends-on ("nodecode-upstash" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-upstash-tests)))
