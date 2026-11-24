;;;; nodecode-gemini-live-bridge.asd --- one Gemini Live session, its catalog, /gemini-live-bridge.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gemini-live-bridge' section, registers GEMINI_LIVE_BRIDGE_TALK and
;;;; GEMINI_LIVE_BRIDGE_MODELS and /gemini-live-bridge, and puts the topic on (help
;;;; :gemini-live-bridge).
;;;;
;;;; A Nodecode rewrite of cruzlxyz's gemini-live-bridge (see NOTICE).

(defsystem "nodecode-gemini-live-bridge"
  :description "Gemini Live: one duplex session with the Live API, its model catalog and its token meter"
  :author "cruzlxyz (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "live")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gemini-live-bridge/test"))))

(defsystem "nodecode-gemini-live-bridge/test"
  :description "Gemini Live tests. Registered into the shared nodecode.test registry; RUN-GEMINI-LIVE-TESTS filters by the GEMINI-LIVE-CELL- name prefix."
  :author "cruzlxyz (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-gemini-live-bridge" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gemini-live-bridge-tests)))
