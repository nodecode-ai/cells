;;;; nodecode-kinocut.asd --- local video editing on the ffmpeg this machine
;;;; already has.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `kinocut' section names the binaries, the render folder and how a render
;;;; looks. It is a rewrite of KyaniteLabs' Kinocut plugin for Hermes Agent --
;;;; see NOTICE.

(defsystem "nodecode-kinocut"
  :description "Kinocut: local video editing on your own ffmpeg -- probe, trim, reformat vertical, burn captions, quality-check"
  :license "Apache-2.0"
  :author "KyaniteLabs (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "media")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kinocut/test"))))

(defsystem "nodecode-kinocut/test"
  :description "Tests for the kinocut cell; RUN-KINOCUT-TESTS filters the shared registry by the KINOCUT-CELL- prefix."
  :license "Apache-2.0"
  :author "KyaniteLabs (original), Nodecode (rewrite)"
  :depends-on ("nodecode-kinocut" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kinocut-tests)))
