;;;; nodecode-eyes.asd --- the agent sees what it just made.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `eyes' section names the folder contact sheets go in and the two
;;;; binaries. It is a rewrite of Agenziamaia's eyes plugin for Hermes Agent
;;;; -- see NOTICE.

(defsystem "nodecode-eyes"
  :description "Eyes: a measured note on every image, video or sound a tool just wrote -- frame filled, motion, brightness, loudness, silences"
  :license "MIT"
  :author "Agenziamaia (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "media")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-eyes/test"))))

(defsystem "nodecode-eyes/test"
  :description "Tests for the eyes cell; RUN-EYES-TESTS filters the shared registry by the EYES-CELL- prefix."
  :license "MIT"
  :author "Agenziamaia (original), Nodecode (rewrite)"
  :depends-on ("nodecode-eyes" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-eyes-tests)))
