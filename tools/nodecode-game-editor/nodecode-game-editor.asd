;;;; nodecode-game-editor.asd --- a Godot 4 project read from its files.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `game-editor' section and installs four tools,
;;;; /game-editor and the manual topic (help :game-editor).
;;;;
;;;; Every dependency is already in the serving image: no Godot, no Python,
;;;; no network. The reads are of the project folder the section names.

(defsystem "nodecode-game-editor"
  :description "A Godot 4 project read from its files: scenes, scripts, settings"
  :license "MIT"
  :version "0.1.0"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "text")
               (:file "project")
               (:file "scene")
               (:file "script")
               (:file "settings")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-game-editor/test"))))

(defsystem "nodecode-game-editor/test"
  :description "Game-editor tests: the .tscn and project.godot readers, the four tools, the refusals."
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-game-editor" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-game-editor-tests)))
