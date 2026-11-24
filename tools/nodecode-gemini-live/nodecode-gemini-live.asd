;;;; nodecode-gemini-live.asd --- Gemini Live voice as a Nodecode tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery. It is
;;;; a rewrite of skappafrost's hermes-gemini-live plugin -- see NOTICE.

(defsystem "nodecode-gemini-live"
  :description "Gemini Live voice: one spoken turn per call, the reply saved as a WAV, and the work the voice model delegates named back to you"
  :license "MIT"
  :author "skappafrost (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gemini-live/test"))))

(defsystem "nodecode-gemini-live/test"
  :description "Tests for the gemini-live cell; RUN-GEMINI-LIVE-TESTS filters the shared registry by the GEMINI-LIVE- prefix."
  :license "MIT"
  :author "skappafrost (original), Nodecode (rewrite)"
  :depends-on ("nodecode-gemini-live" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gemini-live-tests)))
