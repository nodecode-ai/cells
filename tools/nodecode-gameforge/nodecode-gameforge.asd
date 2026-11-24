;;;; nodecode-gameforge.asd --- game assets the model can synthesize, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `gameforge' section says where projects are kept, and the five tools
;;;; write into those projects. It is a rewrite of DECRUX9812's GameForge
;;;; plugin for Hermes Agent -- see NOTICE.

(defsystem "nodecode-gameforge"
  :description "Game development toolkit: sound effects, music, pixel sprites, tilesets, engine scaffolds and a web-build exporter, made offline"
  :license "MIT"
  :author "DECRUX9812 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wav")
               (:file "png")
               (:file "audio")
               (:file "gfx")
               (:file "project")
               (:file "zip")
               (:file "export")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gameforge/test"))))

(defsystem "nodecode-gameforge/test"
  :description "Tests for the gameforge cell; RUN-GAMEFORGE-TESTS filters the shared registry by the GAMEFORGE- prefix."
  :license "MIT"
  :author "DECRUX9812 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-gameforge" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gameforge-tests)))
