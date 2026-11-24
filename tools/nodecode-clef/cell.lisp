;;;; cell.lisp --- the primer, the section, the four tools, /clef.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was an MCP package: plugin.json and mcp.json named one
;;;; untrusted streamable-HTTP server, https://clefdrills.com/mcp, and a skill
;;;; under skills/ taught the model when to call it. The cell keeps the same
;;;; four tools and the same server, needs no key, and adds what the skill's
;;;; offline helper gave: chord transposition answered in the cell.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "clef": {"endpoint": "https://clefdrills.com/mcp",
;;;;            "timeout_seconds": 30, "offline_transpose": true}

(in-package #:nodecode-clef)

;;; --- the manual ---------------------------------------------------------------
;;; (help :clef) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "Clef is reachable through the nodecode-clef cell: four model tools, find_music_guides,
transpose_chords, spell_piano_chord and tab_to_piano_notes, and the same four calls in
the CLEF package from eval. No account and no key: the tools ask Clef's public MCP
server at clefdrills.com, and what you send it is the chords, tab or words you give it.
  (clef:transpose \"C G Am F\" 2 &key prefer-flats)   the chords moved, in the cell
  (clef:spell-chord \"Cmaj7\" :octave 4)              the pitches, with MIDI values
  (clef:tab-notes \"e|-0-| ...\" :capo 0)              ASCII guitar tab as piano notes
  (clef:guides \"guitar\")                             Clef's instructional guides
Doctrine: ask for the smallest call that answers the question. Transposition with
offline_transpose on (the default) sends nothing anywhere; the other three do. Say
which direction and which spelling you transposed into. Tab spacing orders columns,
it does not give rhythm, so never claim durations, audio analysis, Guitar Pro files or
generated notation. When the server refuses, CLEF-ERROR carries its own sentence."
  "What (help :clef) answers while the cell runs.")

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell clef
  (:section ("clef")
    (:guide "no key is needed: endpoint is Clef's public MCP server and timeout_seconds bounds one call; offline_transpose answers transpose_chords in the cell, without asking the server at all")
    ("endpoint" :string :default "https://clefdrills.com/mcp"
                :doc "the MCP endpoint the three server tools and the guide search are asked")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "seconds one call to Clef's server may take")
    ("offline_transpose" :boolean :default t
                         :doc "answer transpose_chords in the cell, as the plugin's offline helper did, instead of asking the server"))
  (:tool "find_music_guides" (nlk:contract 'guide-request) #'guide-tool)
  (:tool "transpose_chords" (nlk:contract 'transpose-request) #'transpose-tool)
  (:tool "spell_piano_chord" (nlk:contract 'spell-request) #'spell-tool)
  (:tool "tab_to_piano_notes" (nlk:contract 'tab-request) #'tab-tool)
  (:command "clef" 'run-slash
            :catches 'clef-error
            :description "Transpose chords, spell a piano chord or find Clef's guides"
            :argument-hint "+2 C G Am F")
  (:help :clef
   "clef: transpose chords, spell piano chords, map guitar tab to piano notes and find Clef's guides"
   *primer*))
