;;;; nodecode-hud-teach.asd --- numbered marks and a step card on the live Mac window.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `hud-teach' section names where screenshots are written, which Tcl/Tk
;;;; paints and how long the marks stay. It is a rewrite of iamlukethedev's
;;;; HUD Teach plugin for Hermes Agent -- see NOTICE.

(defsystem "nodecode-hud-teach"
  :description "HUD Teach: numbered marks and a step card painted on the live Mac window, so a lesson is shown on the app itself"
  :license "MIT"
  :author "iamlukethedev (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "screen")
               (:file "shapes")
               (:file "overlay")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hud-teach/test"))))

(defsystem "nodecode-hud-teach/test"
  :description "Tests for the hud-teach cell; RUN-HUD-TEACH-TESTS filters the shared registry by the HUD-TEACH- prefix."
  :license "MIT"
  :author "iamlukethedev (original), Nodecode (rewrite)"
  :depends-on ("nodecode-hud-teach" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hud-teach-tests)))
