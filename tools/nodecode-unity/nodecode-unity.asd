;;;; nodecode-unity.asd --- a Unity project in the open editor, as four tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `unity' section, registers the four tools cell.lisp lists and
;;;; puts the plugin's guidance on the manual as (help :unity).
;;;;
;;;; A rewrite of TakeshiTGAL's hermes-plugin-csharp-game-editor (see NOTICE):
;;;; the original wrapped MCP for Unity in a Python bridge. This cell speaks
;;;; MCP over HTTP to the same MCP for Unity server, in Lisp, and keeps the
;;;; part that made the original worth having: one approved plan per change.

(defsystem "nodecode-unity"
  :description "A Unity project in the open editor: list its tools, call one, and run a change as one approved plan"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-unity/test"))))

(defsystem "nodecode-unity/test"
  :description "Unity cell tests. Registered into the shared nodecode.test registry; RUN-UNITY-TESTS filters by the UNITY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-unity" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-unity-tests)))
