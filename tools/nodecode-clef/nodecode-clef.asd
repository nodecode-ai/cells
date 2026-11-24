;;;; nodecode-clef.asd --- Clef's music tools as four model tools and a command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `clef' section, registers the four tools Clef's public MCP
;;;; server lists (find_music_guides, transpose_chords, spell_piano_chord,
;;;; tab_to_piano_notes), puts /clef and the manual topic on, and hands the
;;;; model the primer.
;;;;
;;;; The original is a Hermes Agent plugin: an Agent Plugins v1 package with a
;;;; plugin.json, an mcp.json naming https://clefdrills.com/mcp, and a skill
;;;; with an offline transposition helper. This cell speaks the same MCP
;;;; server's JSON-RPC directly through the image's own HTTP call -- no Python,
;;;; nothing fetched at run time, no dependency of its own -- and answers the
;;;; same answers as lines a model reads. The transposition the plugin's
;;;; helper did offline is ported into chords.lisp.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-clef"
  :description "Clef's music tools: transpose chords, spell piano chords, map guitar tab to piano notes and find music guides"
  :license "MIT"
  :version "0.1.0"
  :author "MelMayssonOwen (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "chords")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-clef/test"))))

(defsystem "nodecode-clef/test"
  :description "Clef's music tools tests. Registered into the shared nodecode.test registry; RUN-CLEF-TESTS filters by the CLEF-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "MelMayssonOwen (original), Nodecode (rewrite)"
  :depends-on ("nodecode-clef" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-clef-tests)))
