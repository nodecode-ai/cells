;;;; nodecode-memory-select.asd --- only the memories that fit the query and the budget.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `memory-select' section, registers the one tool the original
;;;; plugin gave its harness, and puts the manual on (help :memory-select).
;;;;
;;;; The original is a Hermes Agent plugin: an MCP tool, memory_select, that
;;;; ranked a caller's retrieved memories by lexical relevance, freshness and
;;;; importance and kept what fit a token budget. This cell makes the same
;;;; decision in Lisp, in the image, with no Python and no network at all.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-memory-select"
  :description "Memory selection: keep the retrieved memories that fit the query and the token budget"
  :license "MIT"
  :version "0.1.0"
  :author "Pinutss (original), Nodecode (rewrite)"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "select")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-memory-select/test"))))

(defsystem "nodecode-memory-select/test"
  :description "Memory selection tests. Registered into the shared nodecode.test registry; RUN-MEMORY-SELECT-TESTS filters by the MEMORY-SELECT-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Pinutss (original), Nodecode (rewrite)"
  :depends-on ("nodecode-memory-select" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-memory-select-tests)))
