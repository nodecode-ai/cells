;;;; nodecode-anki.asd --- Anki flashcards through a local AnkiConnect endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `anki' section, registers the five tools the original plugin
;;;; gave its harness (anki_decks, anki_add_cards, anki_search, anki_due,
;;;; anki_answer), puts /anki and the manual topic on, and hands the model a
;;;; primer.
;;;;
;;;; The original is a Hermes Agent plugin: a Python tool that POSTs
;;;; AnkiConnect's JSON API on the operator's own machine. This cell makes
;;;; the same requests through the image's own HTTP call -- no Python,
;;;; nothing fetched at run time, no dependency of its own -- and answers
;;;; the same fields as JSON text a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-anki"
  :description "Anki: cards, search and a quiz loop through a local AnkiConnect endpoint"
  :license "MIT"
  :version "0.1.0"
  :author "suhas-svg (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-anki/test"))))

(defsystem "nodecode-anki/test"
  :description "Anki tests. Registered into the shared nodecode.test registry; RUN-ANKI-TESTS filters by the ANKI-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "suhas-svg (original), Nodecode (rewrite)"
  :depends-on ("nodecode-anki" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-anki-tests)))
