;;;; nodecode-habitude.asd --- your own Markdown journal, searched read-only, with citations.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `habitude' section, registers the four Vault tools, puts the
;;;; manual on (help :habitude) and registers /habitude.
;;;;
;;;; A Nodecode rewrite of legott-ai's Habitude integration for Hermes (see
;;;; NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-habitude"
  :description "Search your own journal or Obsidian notes, read-only, with file and line citations"
  :license "MIT"
  :author "legott-ai (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-habitude/test"))))

(defsystem "nodecode-habitude/test"
  :description "Habitude tests. Registered into the shared nodecode.test registry; RUN-HABITUDE-TESTS filters by the HABITUDE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-habitude" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-habitude-tests)))
