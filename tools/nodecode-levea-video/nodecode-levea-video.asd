;;;; nodecode-levea-video.asd --- Levea's agentic video editor as Nodecode tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `levea-video' section says which key, host and timeouts every call uses.
;;;; It is a rewrite of brajendrak00068's Levea Video Editor plugin for Hermes
;;;; Agent -- see NOTICE.

(defsystem "nodecode-levea-video"
  :description "Levea agentic video editing: describe the edit, the cloud editor renders and exports an MP4"
  :license "MIT"
  :author "brajendrak00068 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "backend")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-levea-video/test"))))

(defsystem "nodecode-levea-video/test"
  :description "Tests for the levea-video cell; RUN-LEVEA-VIDEO-TESTS filters the shared registry by the LEVEA-VIDEO- prefix."
  :license "MIT"
  :author "brajendrak00068 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-levea-video" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-levea-video-tests)))
